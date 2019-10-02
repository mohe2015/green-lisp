#include <termios.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdint.h>
#include <unistd.h>
#include <strings.h>

int main(void) {
	struct termios oldtio{}, newtio{};
	int fd = open("/dev/serial/by-id/usb-16c0_092e-if00", O_RDWR | O_NOCTTY);
        if (fd < 0) {
            perror("device not found");
            exit(-1);
        }
	tcgetattr(fd, &oldtio); /* save current port settings */

    bzero(&newtio, sizeof(newtio));

    // TODO autodetect from /dev/tty device properties
    //if (xbeeInUse) {
    //    newtio.c_cflag = B9600 | CRTSCTS | CS8 | CLOCAL | CREAD;
    //} else {
        newtio.c_cflag = B9600 | CRTSCTS | CS8 | CLOCAL | CREAD | PARENB;
    //}
    newtio.c_iflag = IGNPAR;
    newtio.c_oflag = 0;

    /* set input mode (non-canonical, no echo,...) */
    newtio.c_lflag = 0;

    newtio.c_cc[VTIME] = 0;   /* inter-character timer unused */
    newtio.c_cc[VMIN] = 1;   /* blocking read until 1 char received */

    tcflush(fd, TCIFLUSH);
    tcsetattr(fd, TCSANOW, &newtio);


	while (1) {
uint8_t buf;
        ssize_t readLen = read(fd, &buf, 1);
	if (readLen == 1) {	
		printf("%c", buf);
	}
}

//write(fd, &b, 1);






 tcdrain(fd);
    tcsetattr(fd, TCSANOW, &oldtio);
    close(fd);

}

