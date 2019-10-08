#include <termios.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <stdbool.h>

struct serial {
  int fd;
  struct termios oldtio;
  struct termios newtio;
};

struct serial* serial_connect() {
  struct serial* serial = calloc(1, sizeof(struct serial));
  serial->fd = open("/dev/serial/by-id/usb-16c0_092e-if00", O_RDWR | O_NOCTTY);
  if (serial->fd < 0) {
    perror("device not found");
    exit(-1);
  }
  tcgetattr(serial->fd, &serial->oldtio); /* save current port settings */

  // TODO autodetect from /dev/tty device properties
  //if (xbeeInUse) {
  //    newtio.c_cflag = B38400 | CRTSCTS | CS8 | CLOCAL | CREAD;
  //} else {
  serial->newtio.c_cflag = B38400 | CRTSCTS | CS8 | CLOCAL | CREAD | PARENB;
  //}
  serial->newtio.c_iflag = IGNPAR;
  serial->newtio.c_oflag = 0;

  /* set input mode (non-canonical, no echo,...) */
  serial->newtio.c_lflag = 0;

  serial->newtio.c_cc[VTIME] = 0;   /* inter-character timer unused */
  serial->newtio.c_cc[VMIN] = 1;   /* blocking read until 1 char received */

  tcflush(serial->fd, TCIFLUSH);
  tcsetattr(serial->fd, TCSANOW, &serial->newtio);

  return serial;
}
 
int serial_read(struct serial* serial) {
  uint8_t c;
  ssize_t readLen = read(serial->fd, &c, 1);
  if (readLen == 1) {	
    return c;
  } else {
    return -1;
  }
}
 
bool serial_write(struct serial* serial, uint8_t c) {
  ssize_t writeLen = write(serial->fd, &c, 1);
  return writeLen == 1;
}

void serial_close(struct serial* serial) {
  tcdrain(serial->fd);
  tcsetattr(serial->fd, TCSANOW, &serial->oldtio);
  close(serial->fd);
  free(serial);
}
