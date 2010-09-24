obj-m += bochsfb.o

ifndef $(KDIR)
KDIR_RAW=$(shell uname -r | sed 's/\-.*$$//')
 ifneq ($(KDIR_RAW),)
  KDIR=/usr/src/linux-$(KDIR_RAW)
 endif
endif

ifneq ($(KDIR),)
 KDIR=/usr/src/linux
endif

all: bochsfb.ko

bochsfb.ko: bochsfb.c
	make -C $(KDIR) M=$(PWD) modules

install:
	make -C $(KDIR) M=$(PWD) modules_install

clean:
	make -C $(KDIR) M=$(PWD) clean
	rm -f modules.order test_info *~ shit glutton info claim

load:
	modprobe fbcon >/dev/null 2>&1 || true
	rmmod bochsfb || true
	insmod bochsfb.ko

unload:
	rmmod bochsfb || true

