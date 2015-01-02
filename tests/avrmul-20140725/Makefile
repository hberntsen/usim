CC = /usr/bin/avr-gcc
CFLAGS = -g -Wall  -O3 -I./include/ -DF_CPU=16000000 -DNTESTS=1000
AVROBJCOPY = /usr/bin/avr-objcopy
AVROBJDUMP = /usr/bin/avr-objdump

all: test/test_mul_atmega2560 \
 		 test/stack_mul_atmega2560 \
		 test/test_mul_atmega328

KARATSUBA_MUL = karatsuba48_branched.S \
 								karatsuba48_branchfree.S \
 								karatsuba64_branched.S \
 								karatsuba64_branchfree.S \
 								karatsuba80_branched.S \
 								karatsuba80_branchfree.S \
 								karatsuba96_branched.S \
 								karatsuba96_branchfree.S \
 								karatsuba128_branched.S \
 								karatsuba160_branchfree.S \
 								karatsuba192_branchfree.S \
 								karatsuba256_branched.S \

ALL_MUL = $(KARATSUBA_MUL) \
 								karatsuba128_branchfree.S \
 								karatsuba192_branched.S \
 								karatsuba160_branched.S \
 								karatsuba256_branchfree.S

test/test_mul_atmega2560: $(ALL_MUL) test/test_mul.c test/avr.c test/print.c test/randombytes.c
	$(CC) $(CFLAGS) -mmcu=atmega2560 $^ -o $@
	$(AVROBJCOPY) -O ihex -R .eeprom test/test_mul_atmega2560 test/test_mul_atmega2560.hex
	$(AVROBJDUMP) -D -z test/test_mul_atmega2560 > test/test_mul_atmega2560.dump

test/stack_mul_atmega2560: $(ALL_MUL) test/stack_mul.c test/avr.c test/print.c test/randombytes.c
	$(CC) $(CFLAGS) -mmcu=atmega2560 $^ -o $@
	$(AVROBJCOPY) -O ihex -R .eeprom test/stack_mul_atmega2560 test/stack_mul_atmega2560.hex
	$(AVROBJDUMP) -D -z test/stack_mul_atmega2560 > test/stack_mul_atmega2560.dump

test/test_mul_atmega328: $(KARATSUBA_MUL) test/test_mul.c test/avr.c test/print.c test/randombytes.c
	$(CC) $(CFLAGS) -mmcu=atmega328 $^ -o $@
	$(AVROBJCOPY) -O ihex -R .eeprom test/test_mul_atmega328 test/test_mul_atmega328.hex
	$(AVROBJDUMP) -D -z test/test_mul_atmega328 > test/test_mul_atmega328.dump

.PHONY: clean

clean:
	-rm test/test_mul_atmega2560
	-rm test/test_mul_atmega2560.hex
	-rm test/test_mul_atmega2560.dump
	-rm test/test_mul_atmega328
	-rm test/test_mul_atmega328.hex
	-rm test/test_mul_atmega328.dump
	-rm test/stack_mul_atmega2560
	-rm test/stack_mul_atmega2560.hex
	-rm test/stack_mul_atmega2560.dump
