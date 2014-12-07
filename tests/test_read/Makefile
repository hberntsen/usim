CC = /usr/bin/avr-gcc
CFLAGS = -g -Wall -mmcu=atmega2560 -O3 -I./include/ -DF_CPU=16000000 -DNTESTS=10
AVROBJCOPY = /usr/bin/avr-objcopy


test_read_atmega2560.hex:  avr.c avr.h print.c print.h read.c read.h test_read.c
	$(CC) $(CFLAGS) avr.c print.c read.c test_read.c -o test_read_atmega2560


.PHONY: clean

clean:
	-rm test_read_atmega2560

