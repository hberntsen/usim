#!/bin/bash

declare -a files
files[0]="tests/test_write/test_write_atmega2560.dump"
files[1]="tests/test_read/test_read_atmega2560.dump"
files[2]="tests/helloworld-attiny10/main_attiny10.dump"
files[3]="tests/avrmul-20140725/test/test_mul_atmega2560.dump"
#files[4]="tests/avradd/test_add32_atmega2560.dump"
#files[5]="tests/small/test/test.dump"
#files[6]="tests/avrnacl-20130514/highspeed/test/test_nacl_atmega2560.dump"

echo "Executing unittests"
rdmd -unittest main.d --file /dev/null

for f in ${files[@]}; do
	if [ -f $f ]
	then
		echo "Testing $f"
		rdmd main.d --file $f
	else
		echo "Test file $f not found"
	fi
done
