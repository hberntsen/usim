CC = /usr/bin/avr-gcc
CFLAGS = -g -Wall -mmcu=attiny10 -Os -I./include/ -DF_CPU=1000000
AVROBJDUMP = /usr/bin/avr-objdump

test_attiny10.hex: print.c print.h main.c
	$(CC) $(CFLAGS) print.c main.c -o main_attiny10
	$(AVROBJDUMP) -D -z main_attiny10 > main_attiny10.dump

.PHONY: clean

clean:
	-rm test_attiny10
	-rm test_attiny10.dump

