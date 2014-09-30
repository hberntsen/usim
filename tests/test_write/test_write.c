#include "avr.h"
#include "print.h"

int main() {
	unsigned char a = 'a';
	write_byte(a);
	return 0;
}
