#ifndef __ALTINTEL9XXFB_H
#define __ALTINTEL9XXFB_H

#ifndef THIS_IS_KERNEL_MODE
# include <stdint.h>
#endif

/* IOCTLs specific to this driver */

/* Virtualbox requires a FLUSH command on secondary displays */
#define BOCHSFB_FLUSH		_IO('B', 1)

#endif

