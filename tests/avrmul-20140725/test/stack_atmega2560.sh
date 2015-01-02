#!/bin/sh
DEVICE=/dev/ttyACM0
DIR=`dirname $0`

avrdude -cstk500v2 -p atmega2560 -P $DEVICE -U flash:w:$DIR/stack_mul_atmega2560.hex -v
stty -F $DEVICE raw icanon eof \^d 38400
cat < $DEVICE 
