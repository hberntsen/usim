CC = /usr/bin/avr-gcc
CFLAGS = -g -Wall -mmcu=atmega2560 -O3 -I./include/ -DF_CPU=16000000 -DNTESTS=10
AVROBJCOPY = /usr/bin/avr-objcopy
AVROBJDUMP = /usr/bin/avr-objdump


test_write_atmega2560.hex:  avr.c avr.h print.c print.h test_write.c
	$(CC) $(CFLAGS) avr.c print.c test_write.c -o test_write_atmega2560
	$(AVROBJDUMP) -D test_write_atmega2560 > test_write_atmega2560.dump


.PHONY: clean

clean:
	-rm test_write_atmega2560

