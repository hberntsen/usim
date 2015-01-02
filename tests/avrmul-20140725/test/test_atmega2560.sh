#!/bin/sh
DEVICE=/dev/ttyACM0
DIR=`dirname $0`

avrdude -cstk500v2 -p atmega2560 -P $DEVICE -U flash:w:$DIR/test_mul_atmega2560.hex -v
stty -F $DEVICE raw icanon eof \^d 38400
echo "===== Starting Test; any output means something is wrong ====="
cat < $DEVICE | sed "s/\r//" | bc | grep -v ^0$
