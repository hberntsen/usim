# μsim

*A fast and extensible microcontroller simulator and debugger in D.*

## Why

When designing and implementing algorithms for microcontrollers, testing your code may take a lot of time and debugging may be hard. Each iteration requires you to load the code on the physical platform. Then the chip itself can be slow, too. Finally, debugging with only a serial port can be a pain.

With this in mind, this simulator and debugger is developed, that allows you to test your AVR code faster than real-time on your pc. It is designed in such a way that it is easy to extend support to other microarchitectures.

## Usage

### Compiling
To compile our code, you first need a [D compiler](http://dlang.org/download.html). We find that [ldc](https://github.com/ldc-developers/ldc) yields the fastest results, but if you prefer `dmd` or `gdc`, those are fine as well. Our Makefile uses `ldc2`, though. To compile our code using `ldc2`, simply run `make`.

### Preparing input
μsim does not not run on a binary, but on the format produced by `objdump`. Compile the code that you want to run on the simulator and do the following.
```bash
$ avr-objdump -D -z myprogram > myprogram.dump
```
The -D and -z flags are necessary to prevent errors while simulating the code. Also make sure that your avr-objdump version at least 2.24.

### Running modes
μsim supports three running modes.

**Simple mode** μsim can be run directly on a file, like this.
```bash
$ ./usim myprogram.dump
```
Instead of simulating all hardware details, serial output is written to stdout, just like serial input is read from stdin, for easy debugging purposes.

**Debug mode** Second, μsim can be run in debugger mode.
```bash
$ ./usim --debug myprogram.dump
```
The program now starts listening on a TCP port, 3742 by default. The debugger interface may connect with `./debug.py`, assuming `python` links to at least Python 3.3.

**Batch mode** μsim also supports a batch mode for simulating a lot at once.
```bash
$ ./usim --batch myprogram.dump
```

### Options

`./usim` lists all available options for the simulator. Noteworthy is that initial raw memory contents may be read from files, just as the final state of the raw memory may be written to files.

For the available debugger commands, simply enter `help`.

## Extending to other platforms

Currently, the following platforms are supported out-of-the-box:

* ATMega328
* ATMega88
* ATMega2560
* ATTiny10

To add a new AVR chipset, add a new enumeration value in `spec/avrchips.d` and create a new AvrFactory with that AvrChipSpec.

To extend the simulator to a non-AVR microarchitecture, a new D module in `spec` must be written that explains how the machine state and instructions look like and work, similar to `spec/avrstate.d`. A factory for the new machine must then be added to machineFactories.

## About

This project is developed by [hberntsen](https://github.com/hberntsen), [efjboss](https://github.com/efjboss), [HansH](https://github.com/HansH) and [Ko-](https://github.com/Ko-) as part of a university project for [Peter Schwabe](https://cryptojedi.org/peter/).
