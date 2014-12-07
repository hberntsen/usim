#include "avr.h"
#include "print.h"
#include "read.h"

int main() {
	char c = 0;
	write_byte('S');
	write_byte('t');
	write_byte('a');
	write_byte('r');
	write_byte('t');
	write_byte(':');
	write_byte(' ');
	while(c != 'q') {
		c = read_byte();
		write_byte(c);
	}
	return 0;
}
