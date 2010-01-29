#ifndef __ALTINTEL9XXFB_H
#define __ALTINTEL9XXFB_H

#ifndef THIS_IS_KERNEL_MODE
# include <stdint.h>
#endif

/* IOCTLs specific to this driver */
#define BOCHSFB_DUMMY		_IOW('B', 1, unsigned int)

#endif

