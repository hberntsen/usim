# μsim

*A fast and extensible microcontroller simulator and debugger in D.*

## Why

When designing and implementing algorithms for microcontrollers, testing your code may take a lot of time and debugging may be hard. Each iteration of your code requires you to load the code on the physical platform. Then the chip itself can be slow as well. Finally, debugging with only a serial port can be a serious pain.

With this in mind, this simulator and debugger is developed, that allows you to test your AVR code faster than real-time on your pc. It is designed in such a way that it is easy to extend support to other microarchitectures.

## Usage

### Compiling
To compile our code, you first need a [D compiler](http://dlang.org/download.html). We find that [ldc](https://github.com/ldc-developers/ldc) yields the fastest results, but if you prefer `dmd` or `gdc`, those are fine as well. Our Makefile uses `ldc2`, though. To compile our code using `ldc2`, simply run `make`.

### Preparing input
μsim does not run on a binary, but on the format produced by `avr-objdump`. Compile the code that you want to run on the simulator and do the following.
```bash
$ avr-objdump -D -z myprogram > myprogram.dump
```
The -D and -z flags are necessary to disassemble everything that is needed to simulate the code. Also make sure that your avr-objdump version is at least 2.20 (or 2.23.1-1 when using the Debian package), but preferably at least 2.24. Earlier versions contain bugs that may ask the simulator to execute undefined behaviour.

### Running modes
μsim supports three different running modes.

**Simple mode**

μsim can be run directly on a file, like this.
```bash
$ ./usim myprogram.dump
```
This simulates the ATMega2560 microcontroller while running your program and shows the exact number of cycles that were used. Some hardware details are omitted, such as timers. Furthermore, serial output is written to stdout, just like serial input is read from stdin, for easy debugging purposes. The simulator will never have to wait for the serial port to be ready.

**Debug mode**

Secondly, μsim can be run in debugger mode.
```bash
$ ./usim --debug myprogram.dump
```
The program now starts listening on a TCP port. 3742 by default, but this can be changed with `--port`. Commands can be sent to this port, but we also provide a clear curses GUI. The debugger interface may connect with `./debug.py`, assuming your default Python version is at least 3.3. Enter `help` to view the list of available debugger commands.

**Batch mode**

μsim also supports a batch mode for simulating a lot of tests in parallel. This mode assumes that input will be provided on stdin.
```bash
$ cat myinputs | ./usim --batch --count $(wc -l < myinputs) myprogram.dump
```

μsim by default assumes that a newline (\n) is used to separate your input, but this can be changed with `--separator`.

### More options

`./usim` lists all configurable options for the simulator. Noteworthy is that initial raw memory contents may be read from files, just as the final state of the raw memory may be written to files. E.g. `--memfile eeprom=myeeprom.bin data=mydata.bin`. Finally, a different microcontroller can be selected using, for instance, `--mcu atmega328` and the statistics can be hidden with `--stats=false`.

## Extending to other platforms

Currently, the following platforms are supported out-of-the-box:

* ATMega328
* ATMega88
* ATMega2560
* ATTiny10

To add a new AVR chipset, add a new enumeration value in `spec/avrchips.d` and create a new AvrFactory with that AvrChipSpec. This should be only a few lines of code.

To extend the simulator to a non-AVR microarchitecture, a new D module in `spec` must be written that explains how the machine state and instructions look like and work, similar to `spec/avrstate.d`. A factory for the new machine must then be added to machineFactories.

## About

This project is developed by [hberntsen](https://github.com/hberntsen), [efjboss](https://github.com/efjboss), [HansH](https://github.com/HansH) and [Ko-](https://github.com/Ko-) as part of a university project for [Peter Schwabe](https://cryptojedi.org/peter/).
