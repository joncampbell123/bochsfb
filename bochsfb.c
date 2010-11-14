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

typedef struct VBVACONF32 {
    uint32_t	u32Index,u32Value;
} __attribute__((packed)) VBVACONF32;

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
static unsigned int		apert_length;
static unsigned int		apert_usable;
static unsigned int		monitors = 1;
static unsigned int		vbox_extensions = 0;
static void*			aperture_stolen = NULL;
static void*			hgsmi_command = NULL;
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

static void bochs_unregister_fb(void) {
	struct fb_info *f;
	unsigned int head;

	for (head=0;head < monitors;head++) {
		if ((f = bochs_fb[head]) != NULL) {
			unregister_framebuffer(f);	/* the code will not leave it in if it didn't succeed */
			fb_dealloc_cmap(&f->cmap);
			framebuffer_release(f);
			bochs_fb[head] = NULL;
		}
	}
}

static void bochs_unmap_resources(void) {
	if (aperture_stolen != NULL) {
		iounmap(aperture_stolen);
		aperture_stolen = NULL;
#if 0
		/* assumption: we wouldn't have ioremap()'d the aperture
		 * if we were not able to claim it */
		release_mem_region(apert_base,apert_usable);
#endif
	}
}

static int bochs_map_resources(void) {
#if 0
	if (!request_mem_region(apert_base,apert_usable,"bochs VBE aperture")) {
		printk(KERN_ERR "bochsfb: cannot claim aperture %llx-%llx\n",
			apert_base,apert_base+apert_usable-1LL);
		bochs_unmap_resources();
		return 1;
	}
#endif

	/* don't ask for the entire aperture, it might be too large for ioremap()
	 * to handle and there's not that much stolen memory anyhow */
	if ((aperture_stolen = ioremap(apert_base,apert_usable)) == NULL) {
		bochs_unmap_resources();
		return 1;
	}

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
/*	struct bochs_fb_info *par = info->par; */
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
	if ((bypsl * var->yres) > apert_usable)
		return -EINVAL;

	var->xres_virtual = bypsl / (var->bits_per_pixel/8);
	var->yres_virtual = apert_usable / bypsl;
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
/*	struct bochs_fb_info *par = info->par; */
	int bypsl;

	bypsl = var->xres_virtual * (var->bits_per_pixel/8);
	info->fix.line_length = bypsl;

	bochs_vbe_w(VBE_DISPI_INDEX_XRES,var->xres);
	bochs_vbe_w(VBE_DISPI_INDEX_YRES,var->yres);
	if (var->bits_per_pixel == 16 && var->red.offset != 11)
		bochs_vbe_w(VBE_DISPI_INDEX_BPP,15);
	else
		bochs_vbe_w(VBE_DISPI_INDEX_BPP,var->bits_per_pixel);
	bochs_vbe_w(VBE_DISPI_INDEX_VIRT_WIDTH,var->xres_virtual);
	bochs_vbe_w(VBE_DISPI_INDEX_ENABLE,VBE_DISPI_ENABLED | VBE_DISPI_LFB_ENABLED | VBE_DISPI_8BIT_DAC);

	return 0;
}

static int bochs_ioctl(struct fb_info *info, unsigned int cmd, unsigned long arg)
{
/*	struct bochs_fb_info *par = info->par; */
	int err = -EINVAL;

	return err;
}
				
static struct fb_ops bochs_ops = {
	.owner =		THIS_MODULE,
	.fb_setcolreg =		bochs_setcolreg,
	.fb_ioctl =		bochs_ioctl,
	.fb_fillrect =		cfb_fillrect,
	.fb_copyarea =		cfb_copyarea,
	.fb_imageblit =		cfb_imageblit,
	.fb_check_var =		bochs_check_var,
	.fb_set_par =		bochs_set_par
};

static void fill_fb_default_mode(struct fb_info *fb,const int index) {
	struct bochs_fb_info *par = fb->par;
	struct fb_var_screeninfo *v = &fb->var;
	struct fb_fix_screeninfo *f = &fb->fix;
	int xres = default_width,yres = default_height;

	/* caller already took care of the id field */
	f->smem_start = apert_base+par->base;
	f->smem_len = apert_usable;
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
	/*		pci_set_drvdata(pci_vga_dev,f); */
	f->flags = FBINFO_DEFAULT | FBINFO_HWACCEL_DISABLED;
	f->fbops = &bochs_ops;
	f->screen_base = aperture_stolen+base;/*aperture_stolen;*/
	f->screen_size = size;/*apert_usable;*/
	strcpy(f->fix.id,"Bochs VBE");
	fill_fb_default_mode(f,i);

	if (fb_alloc_cmap(&f->cmap,256,0) < 0) {
		printk(KERN_ERR "Cannot alloc cmap for framebuffer\n");
		goto err_out;
	}

	if (register_framebuffer(f)) {
		printk(KERN_ERR "Cannot register framebuffer\n");
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
	bochs_unmap_resources();
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

	if (pci_vga_dev == NULL) {
		bochs_free_dev();
		return NULL;
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

static unsigned int hgsmi_allocd = 0;
static HGSMIBUFFERHEADER *hgsmi_cmd_begin(size_t cblen,void **data) {
	HGSMIBUFFERHEADER *h = (HGSMIBUFFERHEADER*)hgsmi_command;
	if ((cblen+sizeof(HGSMIBUFFERHEADER)+sizeof(HGSMIBUFFERTAIL)) >= 4096)
		return NULL;

	hgsmi_allocd = (unsigned int)cblen;
	memset(h,0,sizeof(HGSMIBUFFERHEADER));
	*data = (void*)((char*)hgsmi_command+sizeof(HGSMIBUFFERHEADER));
	h->u32DataSize = 8;
	return h;
}

static void hgsmi_cmd_submit(void) {
	/* complete tail checksum and submit to H/W */
	HGSMIBUFFERTAIL *t = (HGSMIBUFFERTAIL*)((char*)hgsmi_command+sizeof(HGSMIBUFFERHEADER)+hgsmi_allocd);
	t->u32Reserved = 0;
	t->u32Checksum = HGSMIChecksum(((uint32_t)((char*)hgsmi_command - (char*)aperture_stolen)),(HGSMIBUFFERHEADER*)hgsmi_command,t);
	outl(((uint32_t)((char*)hgsmi_command - (char*)aperture_stolen)),HGSMI_GUEST);
	inl(HGSMI_GUEST);
	inl(HGSMI_GUEST);
	inl(HGSMI_GUEST);
	inl(HGSMI_GUEST);
}

static uint32_t hgsmi_query_conf32(uint32_t index) {
	HGSMIBUFFERHEADER *h;
	void *dp = NULL;

	h = hgsmi_cmd_begin(8,&dp);
	if (h) {
		volatile VBVACONF32 *v = (VBVACONF32*)dp;
		BUG_ON(dp == NULL);
		BUG_ON(sizeof(HGSMIBUFFERHEADER) != 16);
		BUG_ON(sizeof(HGSMIBUFFERTAIL) != 8);
		v->u32Index = 0;		/* VBOX_VBVA_CONF32_MONITOR_COUNT */
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
	hgsmi_command = (void*)((char*)aperture_stolen + apert_usable);
	printk(KERN_DEBUG "Allocating 4KB for HGSMI command buffer\n");

	/* how many monitors? */
	v32 = hgsmi_query_conf32(0);		/* VBOX_VBVA_CONF32_MONITOR_COUNT */
	printk(KERN_INFO "%u monitors\n",v32);
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
	if ((pci_vga_dev = bochs_find_device()) == NULL)
		return -ENODEV;

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

	if (bochs_map_resources()) {
		printk(KERN_ERR "bochsfb: cannot map resources\n");
		bochs_free_dev();
		return -ENODEV;
	}

#if 0
#ifdef CONFIG_MTRR
	/* help performance by covering the aperture with
	 * a write-combining MTRR. it might already be there,
	 * so the function may fail. if it does, video ram access
	 * is slightly slower, who cares? */
	mtrr_add(apert_base,apert_usable,MTRR_TYPE_WRCOMB,1);
#endif
#endif

	/* VirtualBox has extensions of it's own that enable multiple monitors
	 * and a memory-based command/response system */
	if (vbox_extensions) {
		if (vbox_hgsmi) {
			if (!check_hgsmi()) {
				printk(KERN_WARNING "bochsfb: HGSMI functionality disabled\n");
				vbox_hgsmi = 0;
			}
		}
	}

	/* register and alloc a framebuffer for each */
	if (monitors == 1) {
		printk(KERN_ERR "bochsfb: setting up single-monitor setup\n");
		if (bochs_register_fb(0,0,apert_usable)) {
			printk(KERN_ERR "Problem registering framebuffers\n");
			bochs_free_dev();
			return -ENOMEM;
		}
	}
	else {
		/* FIXME */
		monitors = 0;
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

