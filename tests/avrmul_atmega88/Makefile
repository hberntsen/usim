CC = /usr/bin/avr-gcc
CFLAGS = -g -Wall -mmcu=atmega88 -O2 -I./include/ -DF_CPU=1000000 -DNTESTS=1000
AVROBJCOPY = /usr/bin/avr-objcopy
AVROBJDUMP = /usr/bin/avr-objdump

all: test/test_mul_atmega88 \
 		 test/stack_mul_atmega88

KARATSUBA_MUL = mul_asm.S

test/test_mul_atmega88: mul_asm.S test/test_mul.c test/avr.c test/print.c 
	$(CC) $(CFLAGS) $^ -o $@
	$(AVROBJCOPY) -O ihex -R .eeprom test/test_mul_atmega88 test/test_mul_atmega88.hex
	$(AVROBJDUMP) -D -z test/test_mul_atmega88 > test/test_mul_atmega88.dump

test/stack_mul_atmega88: mul_asm.S test/stack_mul.c test/avr.c test/print.c 
	$(CC) $(CFLAGS) $^ -o $@
	$(AVROBJCOPY) -O ihex -R .eeprom test/stack_mul_atmega88 test/stack_mul_atmega88.hex
	$(AVROBJDUMP) -D -z test/test_mul_atmega88 > test/stack_mul_atmega88.dump


.PHONY: clean

clean:
	-rm test/test_mul_atmega88
	-rm test/test_mul_atmega88.hex
	-rm test/test_mul_atmega88.dump
	-rm test/stack_mul_atmega88
	-rm test/stack_mul_atmega88.hex
	-rm test/stack_mul_atmega88.dump
