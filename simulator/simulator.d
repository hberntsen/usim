module simulator.simulator;

import std.algorithm;
import std.stdio;
import std.regex;
import std.datetime : StopWatch, TickDuration;
import machine.state;
import spec.base;
import spec.atmega2560;
import parser.parser;
import std.string;
import std.conv;

struct SimulatorState {
    ulong cycles;
    TickDuration runningTime;

    public string toString() {
        double seconds = to!double(runningTime.usecs)/1e6;
        double frequencymMhz = cycles/seconds/1e6;
        return format("%d cycles\n%d usec\n%f seconds\n%f MHz",
                cycles,runningTime.usecs,seconds,frequencymMhz);
    }
}

interface BatchModeSimulator {
    SimulatorState run();
}

struct DebuggerState {
    size_t[] breakpoints;
}

//Input: initial machine state (code is part of the machine state)
final class Simulator(T) : BatchModeSimulator {
    T machineState;
    SimulatorState simulatorState;
    DebuggerState debuggerState;

    this(T initialState) {
        this.machineState = initialState;
        this.simulatorState = SimulatorState(0);
        this.debuggerState = DebuggerState();
    }

    private string handleShowCommand(string[] parameters) {
        string[string] commands = [
            "register": "show register content for the given registers, or all when none are specified",
            "data": "show data memory content at the given address range",
            "program": "show progam memory content at the given address range",
            "instruction": "show the precise instruction under execution",
            "summary": "show a summary of possibly relevant information",
            "help": "show helpful infomration for `show`"
        ];

        auto commandAbbrev = abbrev(commands.keys);

        string[] registers;
        foreach (idx, register; machineState.valueRegisters) {
            registers ~= format("r%02d %s", idx, register);
        }

        if (parameters.length == 0) {
            // todo: abstract this to the machinestate somewhere
            return format(
                    "cycles:\t%d\nregisters:\t%s\n%s\n%s\n",
                    simulatorState.cycles,
                    registers,
                    machineState.refregs,
                    machineState.currentInstruction,
            );
        }

        Memory mem = machineState.data;

        switch(commandAbbrev[parameters[0]]) {
            case "register":
                if (parameters.length < 2) {
                    return join(registers, "\n") ~ "\n";
                }

                string[] registerSet;
                foreach (idx, param; parameters[1 .. $]) {
                    size_t specifier;
                    if (param[0] == 'r') {
                        specifier = to!size_t(param[1 .. $]);
                    } else {
                        specifier = to!size_t(param[0 .. $]);
                    }
                    registerSet ~= registers[specifier];
                }
                return join(registerSet, "\n") ~ "\n";
            case "program":
                mem = machineState.program;
                goto case;
            case "eeprom":
                mem = machineState.eeprom;
                goto case;
            case "data":
                if (parameters.length < 2) {
                    return "Usage: `show <memory> <begin> <end>`\n";
                }

                size_t begin;
                size_t end;

                if (parameters.length == 2) {
                    begin = end = to!size_t(parameters[1]);
                } else if (parameters.length > 2) {
                    begin = to!size_t(parameters[1]);
                    end = to!size_t(parameters[2]);
                }

                if (begin > end) {
                    return "Begin greater than end, please try again";
                }

                string[] dataSlice;
                foreach (idx, el; mem[begin .. end + 1]) {
                    dataSlice ~= format("0x%06x 0x%02x", idx, el);
                }
                return join(dataSlice, "\n");
            case "help":
            default :
                return format("%(- %s %|\n%)\n", commands);
        }
    }

    private string handleSetCommand(string[] parameters) {
        string[string] commands = [
            "breakpoint": "set a breakpoint by linenumber",
        ];
        auto commandAbbrev = abbrev(commands.keys);

        switch (commandAbbrev[parameters[0]]) {
            case "breakpoint":
                if (parameters.length < 2) {
                    return "Usage: `set breakpoint <linenumber>`\n";
                }
                size_t breakpoint = to!size_t(parameters[1]);
                debuggerState.breakpoints ~= breakpoint;
                return format("Breakpoint set at line %d\n", breakpoint);
            case "help":
            default :
                return format("%(- %s %|\n%)\n", commands);
        }
    }

    public string handleDebugCommand(string command) {
        string[string] commands = [
            "run": "execute the program, ignoring breakpoints and further commands",
            "s": "shortcut for `step`",
            "step": "execute a single instruction",
            "continue": "execute the program until the next breakpoint",
            "set": "configure a setting, see `set help`",
            "show": "show information on the current state, see `show help`", 
            "help":  "show this output",
            "?": "see `help`"
        ];
        auto commandAbbrev = abbrev(commands.keys);

        string[] parts = split(chomp(command));
        if (parts.length < 1) {
            return "No command specified\n";
        }

        if (parts[0] in commandAbbrev) {
            switch(commandAbbrev[parts[0]]) {
                case "run":
                    break;
                case "s":
                case "step":
                    step();
                    break;
                case "continue":
                    continueUntilBreakpoint();
                    break;
                case "set":
                    return handleSetCommand(parts[1..$]);
                case "show":
                    return handleShowCommand(parts[1..$]);
                case "help":
                case "?":
                    return format("%(- %s %|\n%)\n", commands);
                default:
                    assert(false);
            }
        } else {
            return "Unknown command: " ~ parts[0] ~ "\n";
        }

        return "OK\n";
    }

    public SimulatorState run(bool withBreakpoints = false) {
        StopWatch stopWatch;
        try {
            stopWatch.start();
            while(step() != step()) {
                if (withBreakpoints &&
                        canFind(debuggerState.breakpoints, machineState.currentInstruction.token.lineNumber)) {
                    break;
                }
            }
            stopWatch.stop();
        } catch (spec.base.EOFException e) {
            stopWatch.stop();
        } catch (Throwable e) {
            stopWatch.stop();
            stderr.writeln("Error");
            stderr.writeln(e);

            stderr.writeln(machineState.currentInstruction());
            debug stderr.writeln(machineState.currentInstruction().token);
        }
        this.simulatorState.runningTime = stopWatch.peek();
        return this.simulatorState;
    }

    public SimulatorState continueUntilBreakpoint() {
        return run(true);
    }

    size_t step() {
        //writefln("\tPC (before): %x", machineState.programCounter);
        auto instr = this.machineState.fetchInstruction();
        //writefln("\tToken: %s", instr.token);
        //writefln("\tRefrefs (before): %s", machineState.refregs);
        //writefln("\tRegisters (before): %s", machineState.registers);
        //writefln("\tFlags (before): %s", machineState.sreg);
        //writefln("\tSP (before): %s", machineState.stackPointer);
        //writefln("\tStack (before): [%(%x, %)]",
                //machineState.data[machineState.stackPointer+1 .. $]);
        //writef("Applying instruction '%s'", instr.name);
        const ulong cycles = instr.callback(machineState);
        //writefln(" - DONE (%d cycles)", cycles);
        //writefln("\tSP (after): %s", machineState.stackPointer);
        //writefln("\tRefrefs (after): %s", machineState.refregs);
        //writefln("\tRegisters (after): %s", machineState.registers);
        //writefln("\tFlags (after): %s", machineState.sreg);

        this.simulatorState.cycles += cycles;
        return instr.address;
    }
}

unittest {
    import spec.base;
    import spec.atmega2560;
    import parser.parser;
    import spec.atmega2560;
    import spec.avrstate;

    auto state = new AtMega2560State;
    auto sim = new Simulator!AtMega2560State(state);

    InstructionToken tok1 = new InstructionToken(1, 0x00, [0xcf, 0xef], "ldi",
            ["r1", "0xFF"]);
    InstructionToken tok2 = new InstructionToken(2, 0x02, [0xcf, 0xef], "ldi",
            ["r2", "0xAA"]);
    InstructionToken tok3 = new InstructionToken(3, 0x04, [0x02, 0xc0], "rjmp",
            [".-2"]);
    enum AvrChipSpec cs = AvrChipSpec();
    Instruction!(AvrState)[] instrs = [new Ldi(tok1), new Ldi(tok2), new Rjmp!cs(tok3)];

    state.setInstructions(instrs);
    auto simstate = sim.run();
    assert(simstate.cycles==6);
    assert(state.registers[1].bytes[0] == 0xff);
    assert(state.registers[2].bytes[0] == 0xaa);
}
