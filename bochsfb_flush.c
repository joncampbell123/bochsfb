#include <sys/types.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <fcntl.h>

#include "bochsfb.h"

int main(int argc,char **argv) {
	int fd,r;

	if (argc < 2) {
		fprintf(stderr,"bochsfb_flush /dev/fbX\n");
		return 1;
	}

	fd = open(argv[1],O_RDWR);
	if (fd < 0) {
		fprintf(stderr,"Failed to open %s, %s\n",
			argv[1],strerror(errno));
		return 1;
	}

	r=ioctl(fd,BOCHSFB_FLUSH);
	if (r < 0)
		fprintf(stderr,"Flush failed, %s\n",strerror(errno));
	else if (r == 1)
		fprintf(stderr,"Flush not effective on this framebuffer\n");

	close(fd);
	return 0;
}

