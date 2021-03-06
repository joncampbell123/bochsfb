/* bochsfb.c
 *
 * Framebuffer driver to control the Bochs/VirtualBox virtual VGA VBE device
 *
 * (C) 2009-2010 Jonathan Campbell
 *
 * 12/20/2009     Initial version
 *
 */

#define THIS_IS_KERNEL_MODE

#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/errno.h>
#include <linux/string.h>
#include <linux/sched.h>
#include <linux/mm.h>
#include <linux/slab.h>
#include <linux/delay.h>
#include <linux/fb.h>
#include <linux/ioport.h>
#include <linux/init.h>
#include <linux/pci.h>
#include <linux/vmalloc.h>
#include <linux/pagemap.h>
#include <linux/screen_info.h>

#include <asm/io.h>

#ifdef CONFIG_MTRR
#include <asm/mtrr.h>
#endif

#include "bochsfb.h"
#include "config.h"

/* DEBUG: If #define'd, the 4K VBVA is exposed at the end of the fb device's
 *        map. Else, it is mapped out (for obvious safety reasons) */
//#define EXPOSE_VBVA

#define VBVA_MAX_RECORDS	(64)

typedef struct VBVAFLUSH {
	uint32_t	u32Reserved;
} __attribute__((packed)) VBVAFLUSH;

typedef struct VBVAENABLE {
	uint32_t	u32Flags;
	uint32_t	u32Offset;
	int32_t		i32Result;
} __attribute__((packed)) VBVAENABLE;

typedef struct VBVAENABLE_EX {
	VBVAENABLE	Base;
	uint32_t	u32ScreenId;
} __attribute__((packed)) VBVAENABLE_EX;

typedef struct VBVAINFOVIEW {
	uint32_t	u32ViewIndex;
	uint32_t	u32ViewOffset;
	uint32_t	u32ViewSize;
	uint32_t	u32MaxScreenSize;
} __attribute__((packed)) VBVAINFOVIEW;

typedef struct VBVAINFOSCREEN {
	uint32_t	u32ViewIndex;
	int32_t		i32OriginX,i32OriginY;
	uint32_t	u32StartOffset;
	uint32_t	u32LineSize;
	uint32_t	u32Width,u32Height;
	uint16_t	u16BitsPerPixel;
	uint16_t	u16Flags;
} __attribute__((packed)) VBVAINFOSCREEN;

typedef struct VBVACONF32 {
    uint32_t		u32Index,u32Value;
} __attribute__((packed)) VBVACONF32;

typedef struct VBVARECORD {
    uint32_t		cbRecord;
} __attribute__((packed)) VBVARECORD;

typedef struct VBVAHOSTFLAGS {
    uint32_t		u32HostEvents;
    uint32_t		u32SupportedOrders;
} __attribute__((packed)) VBVAHOSTFLAGS;

typedef struct VBVABUFFER {
    VBVAHOSTFLAGS	hostFlags;
    uint32_t		off32Data;
    uint32_t		off32Free;
    VBVARECORD		aRecords[VBVA_MAX_RECORDS];
    uint32_t		indexRecordFirst;
    uint32_t		indexRecordFree;
    uint32_t		cbPartialWriteThreshold;
    uint32_t		cbData;
    uint8_t		au8Data[1];
} __attribute__((packed)) VBVABUFFER;

typedef struct VBVACMDHDR {
    int16_t		x,y;
    uint16_t		w,h;
} __attribute__((packed)) VBVACMDHDR;

typedef struct _HGSMIBUFFERHEADER
{
    uint32_t    u32DataSize;            /* Size of data that follows the header. */

    uint8_t     u8Flags;                /* The buffer description: HGSMI_BUFFER_HEADER_F_* */

    uint8_t     u8Channel;              /* The channel the data must be routed to. */
    uint16_t    u16ChannelInfo;         /* Opaque to the HGSMI, used by the channel. */

    union {
        uint8_t au8Union[8];            /* Opaque placeholder to make the union 8 bytes. */

        struct
        {                               /* HGSMI_BUFFER_HEADER_F_SEQ_SINGLE */
            uint32_t u32Reserved1;      /* A reserved field, initialize to 0. */
            uint32_t u32Reserved2;      /* A reserved field, initialize to 0. */
        } Buffer;

        struct
        {                               /* HGSMI_BUFFER_HEADER_F_SEQ_START */
            uint32_t u32SequenceNumber; /* The sequence number, the same for all buffers in the sequence. */
            uint32_t u32SequenceSize;   /* The total size of the sequence. */
        } SequenceStart;

        struct
        {                               /* HGSMI_BUFFER_HEADER_F_SEQ_CONTINUE and HGSMI_BUFFER_HEADER_F_SEQ_END */
            uint32_t u32SequenceNumber; /* The sequence number, the same for all buffers in the sequence. */
            uint32_t u32SequenceOffset; /* Data offset in the entire sequence. */
        } SequenceContinue;
    } u;

} __attribute__((packed)) HGSMIBUFFERHEADER;

typedef struct _HGSMIBUFFERTAIL
{
    uint32_t    u32Reserved;        /* Reserved, must be initialized to 0. */
    uint32_t    u32Checksum;        /* Verifyer for the buffer header and offset and for first 4 bytes of the tail. */
} __attribute__((packed)) HGSMIBUFFERTAIL;

static uint32_t hgsmiHashBegin (void)
{
    return 0;
}

static uint32_t hgsmiHashProcess (uint32_t hash,
                                  const void *pvData,
                                  size_t cbData)
{
    const uint8_t *pu8Data = (const uint8_t *)pvData;

    while (cbData--)
    {
        hash += *pu8Data++;
        hash += (hash << 10);
        hash ^= (hash >> 6);
    }

    return hash;
}

static uint32_t hgsmiHashEnd (uint32_t hash)
{
    hash += (hash << 3);
    hash ^= (hash >> 11);
    hash += (hash << 15);

    return hash;
}

uint32_t HGSMIChecksum (uint32_t offBuffer,
                        const HGSMIBUFFERHEADER *pHeader,
                        const HGSMIBUFFERTAIL *pTail)
{
    uint32_t u32Checksum = hgsmiHashBegin ();

    BUG_ON(sizeof(HGSMIBUFFERHEADER) != 16);
    BUG_ON(sizeof(HGSMIBUFFERTAIL) != 8);

    u32Checksum = hgsmiHashProcess (u32Checksum, &offBuffer, sizeof (offBuffer));
    u32Checksum = hgsmiHashProcess (u32Checksum, pHeader, sizeof (HGSMIBUFFERHEADER));
    u32Checksum = hgsmiHashProcess (u32Checksum, pTail, offsetof(HGSMIBUFFERTAIL, u32Checksum));

    return hgsmiHashEnd (u32Checksum);
}

#define MAX_HEADS		64

/* Module description/parameters */
MODULE_AUTHOR("Jonathan Campbell <jonathan@impactstudiopro.com>");
MODULE_DESCRIPTION("Framebuffer for Bochs/VirtualBox VBE extensions " BOCHSFB_BUILD_BANNER);
MODULE_LICENSE("GPL");

struct bochs_fb_info {
	unsigned int		monitor;
	unsigned long		base,size;
	uint32_t		pseudo_palette[256];
	unsigned char*		hgsmi_page;	/* WARNING: in VRAM */
};

static unsigned char		vbox_hgsmi = 0,vbox_video = 0;
static struct pci_dev*		pci_vga_dev = NULL;
static resource_size_t		apert_base;
static unsigned int		apert_mapped;
static unsigned int		apert_length;
static unsigned int		apert_usable;
static unsigned int		monitors = 1;
static unsigned int		vbox_hgsmi_heapsize = 0;
static unsigned int		vbox_extensions = 0;
static void*			aperture_stolen = NULL;
static void*			hgsmi_command = NULL;
static void*			hgsmi_base = NULL;
static void*			hgsmi_fence = NULL;
struct fb_info*			bochs_fb[MAX_HEADS];
int				default_width=640,default_height=480;

static int			ioport_index;
static int			ioport_data;

#define HGSMI_HOST			0x3B0
#define HGSMI_GUEST			0x3D0

#define VBE_DISPI_INDEX_ID		0x0
#define VBE_DISPI_INDEX_XRES		0x1
#define VBE_DISPI_INDEX_YRES		0x2
#define VBE_DISPI_INDEX_BPP		0x3
#define VBE_DISPI_INDEX_ENABLE		0x4
#define VBE_DISPI_INDEX_BANK		0x5
#define VBE_DISPI_INDEX_VIRT_WIDTH	0x6
#define VBE_DISPI_INDEX_VIRT_HEIGHT	0x7
#define VBE_DISPI_INDEX_X_OFFSET	0x8
#define VBE_DISPI_INDEX_Y_OFFSET	0x9
#define VBE_DISPI_INDEX_VBOX_VIDEO	0xA
#define VBE_DISPI_INDEX_NB		0xB

#define VBE_DISPI_DISABLED		0x00
#define VBE_DISPI_ENABLED		0x01
#define VBE_DISPI_GETCAPS		0x02
#define VBE_DISPI_8BIT_DAC		0x20
#define VBE_DISPI_LFB_ENABLED		0x40
#define VBE_DISPI_NOCLEARMEM		0x80

static unsigned int bochs_vbe_r(unsigned int index) {
	outw(index,ioport_index);
	return inw(ioport_data);
}

static void bochs_vbe_w(unsigned int index,unsigned int val) {
	outw(index,ioport_index);
	outw(val,ioport_data);
}

static unsigned int hgsmi_allocd = 0;
static HGSMIBUFFERHEADER *hgsmi_cmd_begin(size_t cblen,void **data) {
	HGSMIBUFFERHEADER *h = (HGSMIBUFFERHEADER*)hgsmi_command;
	if (((char*)hgsmi_command+cblen+sizeof(HGSMIBUFFERHEADER)+sizeof(HGSMIBUFFERTAIL)) >= (char*)hgsmi_fence)
		return NULL;

	BUG_ON(sizeof(HGSMIBUFFERHEADER) != 16);
	BUG_ON(sizeof(HGSMIBUFFERTAIL) != 8);

	hgsmi_allocd = (unsigned int)cblen;
	memset(h,0,sizeof(HGSMIBUFFERHEADER)+cblen+sizeof(HGSMIBUFFERTAIL));
	*data = (void*)((char*)hgsmi_command+sizeof(HGSMIBUFFERHEADER));
	h->u32DataSize = cblen;
	return h;
}

static void hgsmi_cmd_submit(void) {
	/* complete tail checksum and submit to H/W */
	HGSMIBUFFERTAIL *t = (HGSMIBUFFERTAIL*)((char*)hgsmi_command+sizeof(HGSMIBUFFERHEADER)+hgsmi_allocd);
	t->u32Reserved = 0;
	t->u32Checksum = HGSMIChecksum(((uint32_t)((char*)hgsmi_command - (char*)aperture_stolen)),(HGSMIBUFFERHEADER*)hgsmi_command,t);
	outl(((uint32_t)((char*)hgsmi_command - (char*)aperture_stolen)),HGSMI_GUEST);
	inl(HGSMI_GUEST);
}

static void bochs_unregister_fb(void) {
	HGSMIBUFFERHEADER *h;
	struct fb_info *f;
	unsigned int head;
	void *dp = NULL;

	for (head=0;head < monitors;head++) {
		if ((f = bochs_fb[head]) != NULL) {
			/* disable VBVA */
			{
				h = hgsmi_cmd_begin(sizeof(VBVAFLUSH),&dp);
				if (h) {
					volatile VBVAFLUSH *v = (VBVAFLUSH*)dp;
					BUG_ON(dp == NULL);
					v->u32Reserved = 0;
					h->u8Flags = 0x00;		/* single buffer */
					h->u8Channel = 0x02;		/* 0x02 = VBVA */
					h->u16ChannelInfo = 0x05;	/* VBVA_FLUSH */
					barrier();
					hgsmi_cmd_submit();
					barrier();
				}
				else {
					printk(KERN_ERR "Failed to alloc HGSMI space for flush\n");
				}

				h = hgsmi_cmd_begin(sizeof(VBVAENABLE_EX),&dp);
				if (h) {
					volatile VBVAENABLE_EX *v = (VBVAENABLE_EX*)dp;
					BUG_ON(dp == NULL);

					v->Base.u32Flags = 8 | 4 | 2;	/* abs | ext | disable */
					v->Base.u32Offset = 0;
					v->Base.i32Result = 111;
					v->u32ScreenId = head;
					h->u8Flags = 0x00;		/* single buffer */
					h->u8Channel = 0x02;		/* 0x02 = VBVA */
					h->u16ChannelInfo = 0x07;	/* VBVA_ENABLE */
					barrier();
					hgsmi_cmd_submit();
					barrier();
				}
				else {
					printk(KERN_ERR "Failed to alloc HGSMI space for info query\n");
				}
			}

			unregister_framebuffer(f);	/* the code will not leave it in if it didn't succeed */
			fb_dealloc_cmap(&f->cmap);
			framebuffer_release(f);
			bochs_fb[head] = NULL;
		}
	}
}

static void vbox_hgsmi_set_info(unsigned int i,unsigned long ofs,unsigned long size) {
	HGSMIBUFFERHEADER *h;
	void *dp = NULL;

	h = hgsmi_cmd_begin(sizeof(VBVAINFOVIEW),&dp);
	if (h) {
		volatile VBVAINFOVIEW *v = (VBVAINFOVIEW*)dp;
		BUG_ON(dp == NULL);
		v->u32ViewIndex = i;
		v->u32ViewOffset = ofs;
		v->u32ViewSize = size;
		v->u32MaxScreenSize = size;
		h->u8Flags = 0x00;		/* single buffer */
		h->u8Channel = 0x02;		/* 0x02 = VBVA */
		h->u16ChannelInfo = 0x03;	/* VBVA_INFO_VIEW */
		barrier();
		hgsmi_cmd_submit();
		barrier();
	}
	else {
		printk(KERN_ERR "Failed to alloc HGSMI space for info query\n");
	}

	/* NTS: We VBVA_ENABLE all monitors except #0, which we disable.
	 *      The reason we do that is that, once VBVA is enabled,
	 *      VirtualBox stops updating from the framebuffer except when
	 *      explicitly told to update. Since the Linux kernel knows
	 *      nothing of the flushing commands nor do most users of fbdev,
	 *      this results in a "frozen screen" which is a bad thing
	 *      when that screen is your framebuffer console. */
	h = hgsmi_cmd_begin(sizeof(VBVAENABLE_EX),&dp);
	if (h) {
		volatile VBVAENABLE_EX *v = (VBVAENABLE_EX*)dp;
		volatile VBVABUFFER *vb;
		BUG_ON(dp == NULL);

		if (i == 0)
			v->Base.u32Flags = 8 | 4 | 2;	/* abs | ext | disable */
		else
			v->Base.u32Flags = 8 | 4 | 1;	/* abs | ext | enable */

#ifdef EXPOSE_VBVA
		printk(KERN_WARNING "Warning: EXPOSE_VBVA dictates I expose the VBVA to userspace\n");
		v->Base.u32Offset = ofs + size - 0x1000;
#else
		v->Base.u32Offset = ofs + size;
#endif
		/* clear the VBVA */
		vb = (VBVABUFFER*)(aperture_stolen+v->Base.u32Offset);
		memset((unsigned char*)vb,0,0x1000);
		vb->cbData = 4096 - sizeof(VBVABUFFER);
		/* DEBUG */
		memcpy(aperture_stolen+v->Base.u32Offset+0x1000-5,"Hello",5);

		v->Base.i32Result = 111;
		v->u32ScreenId = i;
		h->u8Flags = 0x00;		/* single buffer */
		h->u8Channel = 0x02;		/* 0x02 = VBVA */
		h->u16ChannelInfo = 0x07;	/* VBVA_ENABLE */
		barrier();
		hgsmi_cmd_submit();
		barrier();
	}
	else {
		printk(KERN_ERR "Failed to alloc HGSMI space for info query\n");
	}

	h = hgsmi_cmd_begin(sizeof(VBVAFLUSH),&dp);
	if (h) {
		volatile VBVAFLUSH *v = (VBVAFLUSH*)dp;
		BUG_ON(dp == NULL);
		v->u32Reserved = 0;
		h->u8Flags = 0x00;		/* single buffer */
		h->u8Channel = 0x02;		/* 0x02 = VBVA */
		h->u16ChannelInfo = 0x05;	/* VBVA_FLUSH */
		barrier();
		hgsmi_cmd_submit();
		barrier();
	}
	else {
		printk(KERN_ERR "Failed to alloc HGSMI space for flush\n");
	}
}

static uint32_t hgsmi_query_conf32(uint32_t index) {
	HGSMIBUFFERHEADER *h;
	void *dp = NULL;

	h = hgsmi_cmd_begin(sizeof(VBVACONF32),&dp);
	if (h) {
		volatile VBVACONF32 *v = (VBVACONF32*)dp;
		BUG_ON(dp == NULL);
		v->u32Index = index;		/* VBOX_VBVA_CONF32_MONITOR_COUNT */
		v->u32Value = 0;
		h->u8Flags = 0x00;		/* single buffer */
		h->u8Channel = 0x02;		/* 0x02 = VBVA */
		h->u16ChannelInfo = 0x01;	/* VBVA_QUERY_CONF32 */
		barrier();
		hgsmi_cmd_submit();
		barrier();
		return v->u32Value;
	}
	else {
		printk(KERN_ERR "Failed to alloc HGSMI space for monitor query\n");
		return (uint32_t)0xFFFFFFFFUL;
	}
}

static void vbox_hgsmi_set_mode(unsigned int i,VBVAINFOSCREEN *si) {
	HGSMIBUFFERHEADER *h;
	void *dp = NULL;

	h = hgsmi_cmd_begin(sizeof(VBVAINFOSCREEN),&dp);
	if (h) {
		volatile VBVAINFOSCREEN *v = (VBVAINFOSCREEN*)dp;
		BUG_ON(dp == NULL);
		*v = *si;
		h->u8Flags = 0x00;		/* single buffer */
		h->u8Channel = 0x02;		/* 0x02 = VBVA */
		h->u16ChannelInfo = 0x06;	/* VBVA_INFO_SCREEN */
		barrier();
		hgsmi_cmd_submit();
		barrier();
	}
	else {
		printk(KERN_ERR "Failed to alloc HGSMI space for modesetting\n");
	}
}

static void bochs_unmap_aperture(void) {
	if (aperture_stolen != NULL) {
		iounmap(aperture_stolen);
		aperture_stolen = NULL;
	}
}

static int bochs_map_aperture(void) {
	if ((aperture_stolen=ioremap(apert_base,apert_mapped)) == NULL)
		return 1;

	return 0;
}

static int bochs_setcolreg(unsigned regno,unsigned red,unsigned green,unsigned blue,unsigned transp,struct fb_info *info) {
	if (regno < 16) {
		u32 *pal = (u32*)(info->pseudo_palette);

		if (info->var.bits_per_pixel == 16) {
			if (info->var.red.offset == 11) {
				pal[regno] = (red & 0xF800) |
					((green & 0xFC00) >> 5) |
					((blue & 0xF800) >> 11);
			}
			else {
				pal[regno] = ((red & 0xF800) >> 1) |
					((green & 0xF800) >> 6) |
					((blue & 0xF800) >> 11);
			}
		}
		else if (info->var.bits_per_pixel == 32) {
			pal[regno] = ((red >> 8) << 16) |
				((green >> 8) << 8) |
				(blue >> 8);
		}
	}

	return 0;
}

static int bochs_check_var(struct fb_var_screeninfo *var,struct fb_info *info) {
	struct bochs_fb_info *par = info->par;
	int bypsl;

	if (var->xres < 16)
		var->xres = 640;
	if (var->yres < 16)
		var->yres = 480;

	var->xres = (var->xres + 7) & (~7);

	/* FIXME: perhaps it's better not yet to allow
	 *        userspace to set virtual panning */
	var->xres_virtual = var->xres;

	/* we do not support 8-bit pseudocolor at this time,
	 * and non-power of 2 bits like 24-bit are not supported.
	 * TODO: Intel chips can do a 64-bit "half-precision" pixel mode,
	 *       can fbcon handle that? */
	if (var->bits_per_pixel < 15)
		return -EINVAL;
	else if (var->bits_per_pixel <= 16)
		var->bits_per_pixel = 16;
	else if (var->bits_per_pixel <= 24)
		var->bits_per_pixel = 24;
	else
		var->bits_per_pixel = 32;

	bypsl = (var->bits_per_pixel/8) * max(var->xres,var->xres_virtual);
	if (bypsl >= 8192) /* safe limit */
		return -EINVAL;
	if ((bypsl * var->yres) > par->size)
		return -EINVAL;

	var->xres_virtual = bypsl / (var->bits_per_pixel/8);
	var->yres_virtual = par->size / bypsl;
	var->vmode = 0;

	if (var->bits_per_pixel <= 16) {
		if (var->red.offset == 11 || var->green.length == 6) {
			var->red.offset = 11;
			var->red.length = 5;
			var->green.offset = 5;
			var->green.length = 6;
			var->blue.offset = 0;
			var->blue.length = 5;
		}
		else {
			var->red.offset = 10;
			var->red.length = 5;
			var->green.offset = 5;
			var->green.length = 5;
			var->blue.offset = 0;
			var->blue.length = 5;
		}
	}
	else {
		var->red.length = var->green.length = var->blue.length = 8;
		var->red.offset = 16;
		var->green.offset = 8;
		var->blue.offset = 0;
	}

/* Boch's VBE interface doesn't allow controlling refresh rate,
 * left/right margins, etc. */
	var->left_margin = (var->xres / 16);
	var->right_margin = (var->xres / 24);
	var->upper_margin = (var->yres / 20);
	var->lower_margin = (var->yres / 18);
	var->hsync_len = var->xres / 10;
	var->vsync_len = (var->yres + 80) / 81;
	var->pixclock = ((1000000000UL /
		(var->xres + var->left_margin + var->right_margin + var->hsync_len)) * 1000UL) /
		((var->yres + var->upper_margin + var->lower_margin + var->vsync_len) * 60);

	return 0;
}

static int bochs_set_par(struct fb_info *info) {
	struct fb_var_screeninfo *var = &info->var;
	struct bochs_fb_info *par = info->par;
	int bypsl;

	bypsl = var->xres_virtual * (var->bits_per_pixel/8);
	info->fix.line_length = bypsl;

	if (par->monitor == 0) {
		bochs_vbe_w(VBE_DISPI_INDEX_XRES,var->xres);
		bochs_vbe_w(VBE_DISPI_INDEX_YRES,var->yres);
		if (var->bits_per_pixel == 16 && var->red.offset != 11)
			bochs_vbe_w(VBE_DISPI_INDEX_BPP,15);
		else
			bochs_vbe_w(VBE_DISPI_INDEX_BPP,var->bits_per_pixel);
		bochs_vbe_w(VBE_DISPI_INDEX_VIRT_WIDTH,var->xres_virtual);
		bochs_vbe_w(VBE_DISPI_INDEX_ENABLE,VBE_DISPI_ENABLED | VBE_DISPI_LFB_ENABLED | VBE_DISPI_8BIT_DAC);
	}

	if (vbox_hgsmi) {
		VBVAINFOSCREEN s;
		BUG_ON(par->monitor >= monitors);

		s.u32ViewIndex = par->monitor;
		s.i32OriginX = 0;
		s.i32OriginY = 0;
		s.u32StartOffset = 0;
		s.u32LineSize = bypsl;
		s.u32Width = var->xres;
		s.u32Height = var->yres;
		s.u16Flags = 1;		/* active */
		if (var->bits_per_pixel == 16 && var->red.offset != 11)
			s.u16BitsPerPixel = 15;
		else
			s.u16BitsPerPixel = var->bits_per_pixel;

		vbox_hgsmi_set_mode(par->monitor,&s);
		vbox_hgsmi_set_info(par->monitor,par->base,par->size);
	}

	return 0;
}

static int hgsmi_flush_rect(struct fb_info *info,unsigned int x,unsigned int y,unsigned int w,unsigned int h) {
	struct bochs_fb_info *par = info->par;
	volatile VBVABUFFER *vb;
	HGSMIBUFFERHEADER *hdr;
	int err = -EINVAL;
	void *dp = NULL;

	if (vbox_hgsmi && par->monitor != 0) {
		volatile VBVACMDHDR *chdr;
		volatile VBVARECORD *rec;

#ifdef EXPOSE_VBVA
		vb = (VBVABUFFER*)(aperture_stolen+par->base+
			par->size-0x1000);
#else
		vb = (VBVABUFFER*)(aperture_stolen+par->base+par->size);
#endif
		/* keep it simple: always reset the offset/record
		 * pointers to zero so we never have to worry about
		 * partial records. */
		vb->off32Data = vb->off32Free = 0;
		vb->indexRecordFirst = vb->indexRecordFree = 0;

		/* make up a single rect */
		chdr = (VBVACMDHDR*)(vb->au8Data+vb->off32Free);
		chdr->x = x;
		chdr->y = y;
		chdr->w = w;
		chdr->h = h;

		/* make up a single record */
		rec = &(vb->aRecords[vb->indexRecordFree]);
		rec->cbRecord = sizeof(VBVACMDHDR);

		/* apply the record */
		vb->indexRecordFree++;
		vb->off32Free += rec->cbRecord;

		hdr = hgsmi_cmd_begin(sizeof(VBVAFLUSH),&dp);
		if (hdr) {
			volatile VBVAFLUSH *v = (VBVAFLUSH*)dp;
			BUG_ON(dp == NULL);
			v->u32Reserved = 0;
			hdr->u8Flags = 0x00;		/* single buffer */
			hdr->u8Channel = 0x02;		/* 0x02 = VBVA */
			hdr->u16ChannelInfo = 0x05;	/* VBVA_FLUSH */
			barrier();
			hgsmi_cmd_submit();
			barrier();
			err = 0;
		}
		else {
			printk(KERN_ERR "Failed to alloc HGSMI space for flush\n");
			err = -ENOMEM;
		}
	}
	else {
		err = 1; /* not effective */
	}

	return err;
}

static int bochs_ioctl(struct fb_info *info, unsigned int cmd, unsigned long arg)
{
	int err = -EINVAL;

	if (cmd == BOCHSFB_FLUSH)
		err = hgsmi_flush_rect(info,0,0,info->var.xres,info->var.yres);
	else if (cmd == BOCHSFB_FLUSH_RECT) {
		struct bochsfb_rect r;

		if (copy_from_user(&r,(const void __user*)arg,sizeof(r)))
			return -EINVAL;

		err = hgsmi_flush_rect(info,r.x,r.y,r.w,r.h);
	}

	return err;
}

void bochs_destroy(struct fb_info *fb) {
	struct bochs_fb_info *par = fb->par;

	printk(KERN_INFO "bochsfb destroy\n");

	if (fb->screen_size != 0) release_mem_region(apert_base+par->base,fb->screen_size);
}
				
static struct fb_ops bochs_ops = {
	.owner =		THIS_MODULE,
	.fb_setcolreg =		bochs_setcolreg,
	.fb_ioctl =		bochs_ioctl,
	.fb_fillrect =		cfb_fillrect,
	.fb_copyarea =		cfb_copyarea,
	.fb_imageblit =		cfb_imageblit,
	.fb_check_var =		bochs_check_var,
	.fb_set_par =		bochs_set_par,
	.fb_destroy =		bochs_destroy
};

static void fill_fb_default_mode(struct fb_info *fb,const int index) {
	struct bochs_fb_info *par = fb->par;
	struct fb_var_screeninfo *v = &fb->var;
	struct fb_fix_screeninfo *f = &fb->fix;
	int xres = default_width,yres = default_height;

	/* caller already took care of the id field */
	f->smem_start = apert_base+par->base;
	f->smem_len = par->size;
	f->type = FB_TYPE_PACKED_PIXELS;
	f->visual = FB_VISUAL_TRUECOLOR;
	f->xpanstep = 1;
	f->ypanstep = 1;
	f->ywrapstep = 0;
	f->line_length = ((xres * (16/8)) + 7) & (~7);
	f->mmio_start = 0;
	f->mmio_len = 0;
	f->accel = FB_ACCEL_NONE;
	v->xres = xres;
	v->yres = yres;
	v->xres_virtual = (f->line_length / (16/8));
	v->yres_virtual = f->smem_len / f->line_length;
	v->xoffset = v->yoffset = 0;
	v->bits_per_pixel = 16;
	v->grayscale = 0;
	v->nonstd = 0;
	v->width = v->height = 0;
	v->left_margin = 40;
	v->right_margin = 24;
	v->upper_margin = 32;
	v->lower_margin = 11;
	v->hsync_len = 96;
	v->vsync_len = 2;
	v->red.offset = 11;
	v->red.length = 5;
	v->green.offset = 5;
	v->green.length = 6;
	v->blue.offset = 0;
	v->blue.length = 5;
	v->pixclock = ((1000000000UL /
		(v->xres + v->left_margin + v->right_margin + v->hsync_len)) * 1000UL) /
		(v->yres + v->upper_margin + v->lower_margin + v->vsync_len) / 60;
	v->vmode = FB_VMODE_NONINTERLACED;
	v->rotate = FB_ROTATE_UR;
}

/* register all framebuffers */
static int bochs_register_fb(unsigned int monitor,unsigned long base,unsigned long size) {
	struct bochs_fb_info *pf;
	struct fb_info *f = NULL;
	int i;

	BUG_ON(monitor >= MAX_HEADS);

	if ((f = framebuffer_alloc(sizeof(struct bochs_fb_info),&pci_vga_dev->dev)) == NULL) {
		printk(KERN_ERR "Not enough memory to allocate a framebuffer\n");
		goto err_out;
	}
	pf = f->par;
	pf->monitor = monitor;
	pf->base = base;
	pf->size = size;
	pf->hgsmi_page = NULL;
	BUG_ON(pf == NULL);
	f->pseudo_palette = pf->pseudo_palette;
	f->flags = FBINFO_DEFAULT | FBINFO_HWACCEL_DISABLED;
	f->fbops = &bochs_ops;
	f->screen_base = aperture_stolen+base;
	f->screen_size = size;
	if (monitors > 1)
		sprintf(f->fix.id,"BochsVBE/%u",monitor+1);
	else
		strcpy(f->fix.id,"Bochs VBE");

	fill_fb_default_mode(f,i);

	if ((f->apertures=alloc_apertures(1)) == NULL) {
		printk(KERN_ERR "Cannot alloc aperture\n");
		goto err_out;
	}
	f->apertures->ranges[0].base = apert_base+pf->base;
	f->apertures->ranges[0].size = f->screen_size;

	if (fb_alloc_cmap(&f->cmap,256,0) < 0) {
		printk(KERN_ERR "Cannot alloc cmap for framebuffer\n");
		goto err_out;
	}

	if (register_framebuffer(f)) {
		printk(KERN_ERR "Cannot register framebuffer\n");
		goto err_out;
	}

	if (request_mem_region(apert_base+pf->base,f->screen_size,"Bochs VBE framebuffer") == NULL) {
		printk(KERN_ERR "request_mem_region failed\n");
		f->screen_size = 0; /* don't let fb_destruct call release_mem_region */
		unregister_framebuffer(f);
		goto err_out;
	}

	bochs_check_var(&f->var,f);
	bochs_set_par(f);
	bochs_fb[monitor] = f;
	f = NULL;

	return 0;
err_out:if (f != NULL) framebuffer_release(f);
	bochs_unregister_fb();
	return 1;
}

static void bochs_free_dev(void) {
	bochs_unmap_aperture();
	if (pci_vga_dev) {
		pci_dev_put(pci_vga_dev);
		pci_vga_dev = NULL;
	}
}

static struct pci_dev *bochs_find_device(void) {
	struct pci_bus *bus = pci_find_bus(0,0);	/* Bochs always keeps it on bus 0 */
	struct pci_dev *dev = NULL;
	int slot;

	if (bus == NULL)
		return NULL;

	for (slot=0;slot < 4;slot++) { /* usually no. 2 or somewhere in that range */
		if ((dev = pci_get_slot(bus,PCI_DEVFN(slot,0))) != NULL) {
			if (!(dev->vendor == 0x80EE || dev->vendor == 0x1234)) {
				pci_dev_put(dev);
				continue;
			}
			
			/* This is designed to match the VGA device regardless of whether it's
			 * listed as VGA or just a display controller */
			if (pci_vga_dev == NULL && (dev->class & 0xFFFF00) == 0x030000) {
				pci_vga_dev = dev;
			}
			else {
				pci_dev_put(dev);
			}
		}
	}

	return pci_vga_dev;
}

static int looks_like_aperture(struct resource *res) {
	if ((res->flags & IORESOURCE_TYPE_BITS) != IORESOURCE_MEM) return 0;	/* must be memory */
	if (!(res->flags & IORESOURCE_PREFETCH)) return 0;			/* must be prefetchable */
	if ((res->end-res->start)+1 < 0x100000) return 0;			/* must be at least 1MB */
	return 1;
}

static int __init bochs_identify_device(void) {
	unsigned int id;
	int i,idx = -1;

	BUG_ON(pci_vga_dev == NULL);

	for (i=0;i < 4;i++) {
	/* okay, now make sure the PCI resources we assumed match up with our expectations */
		if (looks_like_aperture(pci_vga_dev->resource + i)) {
			idx = i;
			break;
		}
	}

	if (idx < 0)
		return 1;

	apert_base = pci_vga_dev->resource[idx].start;
	apert_length = (pci_vga_dev->resource[idx].end - pci_vga_dev->resource[idx].start) + 1;

	/* try to avoid situations where ioremap() fails because someone told
	 * VirtualBox to allocate 64MB of video memory */
	apert_usable = apert_length;
	if (apert_usable > (32*1024*1024))
		apert_usable = (32*1024*1024);

	/* if this is VirtualBox we may be able to make use of multi-monitor */
	bochs_vbe_w(VBE_DISPI_INDEX_ID,0xBE00); /* VBOX_VIDEO */
	bochs_vbe_w(VBE_DISPI_INDEX_ID,0xBE01); /* HGSMI */
	id = bochs_vbe_r(VBE_DISPI_INDEX_ID);
	printk(KERN_DEBUG "VBE ID 0x%04X\n",id);
	vbox_extensions = ((id >= 0xB0C0 && id <= 0xB0C4) || (id >= 0xBE00 && id <= 0xBE02));
	apert_mapped = apert_usable;
	monitors = 1;

	/* if this is VirtualBox, then ask how many monitors there are */
	if (vbox_extensions) {
		vbox_hgsmi = (id == 0xBE01);
		vbox_video = (id == 0xBE00);

		id = bochs_vbe_r(VBE_DISPI_INDEX_VBOX_VIDEO);
		if (id == 0) id = 1;
	}

	return 0;
}

/* NTS: assume caller already checked vbox_hgsmi */
/* NTS: VirtualBox code suggests that the command buffer must reside in
 *      VGA video RAM even though we give the host a raw physical memory
 *      address */
static int check_hgsmi(void) { 
	uint32_t v32;

	BUG_ON(!vbox_hgsmi);

	if (apert_usable < (1*1024*1024))
		return 1;

	apert_usable -= 4096;
	hgsmi_command = hgsmi_base = (void*)((char*)aperture_stolen + apert_usable);
	hgsmi_fence = (void*)((char*)hgsmi_base + 4096);
	printk(KERN_DEBUG "Allocating 4KB for HGSMI command buffer\n");

	/* how many monitors? */
	v32 = hgsmi_query_conf32(0);		/* VBOX_VBVA_CONF32_MONITOR_COUNT */
	if (v32 != 0) v32 = hgsmi_query_conf32(0);
	if (v32 > 0 && v32 <= MAX_HEADS) {
		printk(KERN_INFO "bochsfb: VirtualBox %u monitors present\n",v32);
		monitors = (int)v32;
	}
	else {
		printk(KERN_ERR "bochsfb: Invalid monitor count (%u)\n",v32);
	}

	/* heap size? */
	v32 = hgsmi_query_conf32(1);		/* VBOX_VBVA_CONF32_HOST_HEAP_SIZE */
	if (v32 != 0 && v32 != ~0U) {
		vbox_hgsmi_heapsize = (unsigned int)v32;
		printk(KERN_INFO "bochsfb: HGSMI heapsize %u\n",vbox_hgsmi_heapsize);
	}
	else {
		printk(KERN_ERR "bochsfb: Host failed to properly return heap size (%u)\n",v32);
		return 1;
	}

	return 0;
}

static int __init bochs_detect_ioports(void) {
	static const unsigned int port[] = {0x1CE,0xFF80};
	unsigned int try,i;

	for (i=0;i < (sizeof(port)/sizeof(port[0]));i++) {
		try = port[i];

		outw(0x5555,try);
		if (inw(try) != 0x5555) continue;
		outw(0x1234,try);
		if (inw(try) != 0x1234) continue;

		printk(KERN_DEBUG "Found Bochs VBE controls at I/O port 0x%03X\n",try);
		ioport_index = try;
		ioport_data = try+1;
		return 0;
	}

	return 1;
}

static int __init bochs_init(void)
{
	memset(bochs_fb,0,sizeof(bochs_fb));
	if ((pci_vga_dev=bochs_find_device()) == NULL) {
		bochs_free_dev();
		return -ENODEV;
	}

	if (bochs_detect_ioports()) {
		bochs_free_dev();
		return -ENODEV;
	}

	if (bochs_identify_device()) {
		printk(KERN_ERR "bochsfb: Found possible Intel device but don't know what it is\n");
		bochs_free_dev();
		return -ENODEV;
	}

	if (pci_enable_device(pci_vga_dev)) {
		bochs_free_dev();
		return -ENODEV;
	}

	/* ioremap the range. fail if we cannot ioremap(). we don't
	 * care at this point if request_mem_region() failed. */
	if (bochs_map_aperture()) {
		printk(KERN_ERR "bochsfb: cannot map resources\n");
		bochs_free_dev();
		return -ENODEV;
	}

	/* VirtualBox has extensions of it's own that enable multiple monitors
	 * and a memory-based command/response system */
	if (vbox_extensions) {
		if (vbox_hgsmi) {
			if (check_hgsmi()) {
				printk(KERN_INFO "bochsfb: HGSMI functionality disabled\n");
				vbox_hgsmi = 0;
			}
		}
	}

	/* register and alloc a framebuffer for each */
	if (monitors == 1) {
		printk(KERN_INFO "bochsfb: setting up single-monitor setup\n");
		if (bochs_register_fb(0,0,apert_usable)) {
			printk(KERN_ERR "Problem registering framebuffers\n");
			bochs_free_dev();
			return -ENOMEM;
		}
	}
	else if (monitors > 1) {
		unsigned long per_fb_size = (apert_usable / monitors) & (~0xFFF);
		unsigned long per_fb_alloc = per_fb_size;
		unsigned int i;

#ifndef EXPOSE_VBVA
		/* make room for a VBVA buffer */
		if (vbox_hgsmi && per_fb_alloc >= 0x2000) per_fb_alloc -= 0x1000;
#endif

		printk(KERN_INFO "bochsfb: setting up %u-head output (%lu bytes/monitor)\n",monitors,per_fb_size);
		for (i=0;i < monitors;i++) {
			if (bochs_register_fb(i,i*per_fb_size,per_fb_alloc)) {
				printk(KERN_ERR "Problem registering framebuffer %d\n",i);
				bochs_free_dev();
				return -ENOMEM;
			}
		}
	}

	return 0;
}

static void __exit bochs_exit(void)
{
	bochs_unregister_fb();
	bochs_free_dev();
}

module_init(bochs_init);
module_exit(bochs_exit);

