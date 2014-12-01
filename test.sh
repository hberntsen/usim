#!/bin/bash

declare -a files
files[0]="tests/test_write/test_write_atmega2560.dump"
files[1]="tests/avradd/test_add32_atmega2560.dump"
files[2]="tests/avrmul/test/test_mul_atmega2560.dump"
files[3]="tests/small/test/test.dump"

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
