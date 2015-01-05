module simulator.simulator;

import std.algorithm;
import std.stdio;
import std.regex;
import std.datetime : StopWatch, TickDuration;
import machine.state;
import spec.base;
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
    SimulatorState run(bool withBreakpoints = false);
    string handleDebugCommand(string command);
    @property string file();
    @property string file(string filepath);
    @property MachineState machineState();
    @property MachineState machineState(MachineState newState);
    @property SimulatorState state();
}

struct DebuggerState {
    private string[size_t] breakpoints;
    private ushort nextId = 0;

    public bool hasBreakpoint(size_t line) {
        return (line in this.breakpoints) !is null;
    }

    public string setBreakpoint(size_t line, string name = null) {
        if (name is null) {
            name = format("BR%04d", nextId);
            nextId++;
        }
        this.breakpoints[line] ~= name;
        return name;
    }

    public bool unsetBreakpoint(size_t line) {
        return breakpoints.remove(line);
    }

    public string[] getBreakpoints() {
        string[] repr;
        foreach (idx, key; breakpoints.keys.sort) {
            repr ~= format("%02d. %s (%d)", idx, breakpoints[key], key);
        }

        return repr;
    }
}

//Input: initial machine state (code is part of the machine state)
final class Simulator(T) : BatchModeSimulator {
    private T machineState_;
    SimulatorState simulatorState;
    DebuggerState debuggerState;
    private string file_;
    @property string file() { return file_;}
    @property string file(string newFile) { file_=newFile; return newFile;}
    @property MachineState machineState() { return machineState_; }
    @property MachineState machineState(MachineState newState) { machineState_ = cast(T)(newState); return machineState_; }
    @property SimulatorState state() { return simulatorState; }

    this(T initialState) {
        this.machineState_ = initialState;
        this.simulatorState = SimulatorState(0);
        this.debuggerState = DebuggerState();
    }

    private string handleShowCommand(string[] parameters) {
        string[string] commands = [
            "file": "print the path to the file being debugged",
            "xyz": "show full register contents for the larger X, Y and Z registers",
            "breakpoints": "show all the registered breakpoints",
            "registers": "show register content for the given registers, or all when none are specified",
            "io": "show io register content",
            "flags": "show special flags",
            "data": "show data memory content at the given address range",
            "program": "show program memory content at the given address range",
            "stack": "show stack content at the given address range",
            "instruction": "show the precise instruction under execution",
            "summary": "show a summary of possibly relevant information",
            "help": "show helpful infomration for `show`"
        ];

        auto commandAbbrev = abbrev(commands.keys);

        string[] registers;
        foreach (idx, register; machineState_.registers) {
            registers ~= format("r%02d %s", idx, register);
        }

        if (parameters.length == 0) {
            // todo: abstract this to the machinestate somewhere
            return format(
                    "file:\t%s \ncycles:\t%d \nregs:\t%s \nsp:\t%s \nlast:\t%s \ncurr:\t%s\nsreg:\t%s \n",
                    this.file,
                    simulatorState.cycles,
                    machineState_.refregs,
                    machineState_.stackPointer,
                    machineState_.currentInstruction,
                    machineState_.nextInstruction,
                    machineState_.sreg
            );
        }

        ubyte[] mem = machineState_.data;

        if (parameters[0] in commandAbbrev) {
            switch(commandAbbrev[parameters[0]]) {
                case "io":
                    //return format("%s\n", machineState_.ioRegisters);
                    string[] dataSlice;
                    foreach (idx, el; machineState_.ioRegisters) {
                        auto addr = cast(ushort)(idx + 0x20);
                        dataSlice ~= format("0x%x %s", addr, el);
                    }
                    return join(dataSlice, "\n") ~ "\n";
                case "breakpoints":
                    return join(debuggerState.getBreakpoints(), "\n") ~ "\n";
                case "xyz":
                    auto xyz = machineState_.refregs;
                    return format("X: %s\nY: %s\nZ: %s\n", xyz[0],
                            xyz[1], xyz[2]);
                case "flags":
                    auto flags = machineState_.sreg;
                    ushort[string] repr = [
                        "I": flags.I() ? 1 : 0,
                        "T": flags.T() ? 1 : 0,
                        "H": flags.H() ? 1 : 0,
                        "S": flags.S() ? 1 : 0,
                        "V": flags.V() ? 1 : 0,
                        "N": flags.N() ? 1 : 0,
                        "Z": flags.Z() ? 1 : 0,
                        "Z": flags.Z() ? 1 : 0,
                    ];

                    string[] flagRepr;
                    foreach (idx, key; repr.keys) {
                        flagRepr ~= format("%s: %d", key, repr[key]);
                    }
                    return join(flagRepr, "\n") ~ "\n";
                case "file":
                    return this.file ~ "\n";
                case "stack":
                    auto sp = machineState_.stackPointer;
                    //auto initialSp = cast(ushort)(machineState_.data.size - 2); // TODO configurable?

                    writefln("sp: %s, datalen: %x", sp,
                            cast(ushort)(machineState_.data.length));
                    if (sp.value  + 1 > machineState_.data.length) {
                        return format("Error when reading stack, SP: %s", sp);
                    }

                    ubyte[] stack = machineState_.data[sp.value + 1 .. $];
                    string[] stackRepr;
                    foreach (idx, el; stack) {
                        stackRepr ~= format("0x%06x 0x%02x", sp + idx, el);
                    }

                    return join(stackRepr, "\n") ~ "\n";
                case "registers":
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
                case "instruction":
                    return format("%s\n", machineState_.nextInstruction);
                case "program":
                    mem = machineState_.program;
                    goto case "data";
                case "eeprom":
                    mem = machineState_.eeprom;
                    goto case;
                case "data":
                    if (parameters.length < 2) {
                        return "Usage: `show <memory> <begin> <end>`\n";
                    }

                    size_t begin;
                    size_t end;

                    if (parameters.length >= 2) {
                        if (parameters[1][0 .. 2] == "0x") {
                            string numeric = parameters[1][2 .. $];
                            begin = parse!size_t(numeric, 16);
                        } else {
                            begin = to!size_t(parameters[1]);
                        }
                    }

                    if (parameters.length == 2) {
                        end = begin;
                    } else {
                        if (parameters[2][0 .. 2] == "0x") {
                            string numeric = parameters[2][2 .. $];
                            end = parse!size_t(numeric, 16);
                        } else {
                            end = to!size_t(parameters[2]);
                        }
                    }

                    if (begin > end) {
                        return "Begin greater than end, please try again\n";
                    }

                    string[] dataSlice;
                    foreach (idx, el; mem[begin .. end + 1]) {
                        dataSlice ~= format("0x%06x 0x%02x", idx + begin, el);
                    }
                    return join(dataSlice, "\n") ~ "\n";
                case "help":
                default :
                    return format("%(- %s %|\n%)\n", commands);
            }
        }
        return "Unknown command: " ~ parameters[0] ~ "\n";
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
                debuggerState.setBreakpoint(breakpoint);
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
            "skip": "skip the next instruction",
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
                    run(false);
                    break;
                case "s":
                case "step":
                    step();
                    break;
                case "skip":
                    auto instr = machineState_.fetchInstruction();
                    return format("Skipping instruction: %s\n", instr);
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

    override public SimulatorState run(bool withBreakpoints = false) {
        StopWatch stopWatch;
        try {
            stopWatch.start();
            size_t lastAddress, currentAddress = machineState_.nextInstruction.address;
            do {
                if (withBreakpoints &&
                        debuggerState.hasBreakpoint(
                            machineState_.nextInstruction.token.lineNumber)) {
                    break;
                }
                lastAddress = step();
                currentAddress = machineState_.nextInstruction.address;
                //writefln("last executed: %x, to be executed: %x", lastAddress, currentAddress);
            } while(lastAddress != currentAddress);
            stopWatch.stop();
        } catch (spec.base.EOFException e) {
            stopWatch.stop();
        } catch (Throwable e) {
            stopWatch.stop();
            stderr.writeln("Error");
            stderr.writeln(e);

            stderr.writeln(machineState_.currentInstruction());
            debug stderr.writeln(machineState_.currentInstruction().token);
        }
        this.simulatorState.runningTime = stopWatch.peek();
        return this.simulatorState;
    }

    public SimulatorState continueUntilBreakpoint() {
        return run(true);
    }

    size_t step() {
        //writefln("\tPC (before): %x", machineState_.programCounter);
        auto instr = this.machineState_.fetchInstruction();
        //writefln("\tToken: %s", instr.token);
        //writefln("\tRefrefs (before): %s", machineState_.refregs);
        //writefln("\tRegisters (before): %s", machineState_.registers);
        //writefln("\tFlags (before): %s", machineState_.sreg);
        //writefln("\tSP (before): %s", machineState_.stackPointer);
        //writefln("\tStack (before): [%(%x, %)]",
                //machineState_.data[machineState_.stackPointer+1 .. $]);
        //writef("Applying instruction '%s'", instr.name);
        const ulong cycles = instr.callback(machineState_);
        //writefln(" - DONE (%d cycles)", cycles);
        //writefln("\tSP (after): %s", machineState_.stackPointer);
        //writefln("\tRefrefs (after): %s", machineState_.refregs);
        //writefln("\tRegisters (after): %s", machineState_.registers);
        //writefln("\tFlags (after): %s", machineState_.sreg);

        this.simulatorState.cycles += cycles;
        return instr.address;
    }
}

unittest {
    import spec.base;
    import parser.parser;
    import spec.avrstate;
    enum AvrChipSpec testChip = AvrChipSpec();

    auto state = new AvrState!testChip;
    auto sim = new Simulator!(AvrState!testChip)(state);

    InstructionToken tok1 = new InstructionToken(1, 0x00, [0xcf, 0xef], "ldi",
            ["r1", "0xFF"]);
    InstructionToken tok2 = new InstructionToken(2, 0x02, [0xcf, 0xef], "ldi",
            ["r2", "0xAA"]);
    InstructionToken tok3 = new InstructionToken(3, 0x04, [0x02, 0xc0], "rjmp",
            [".-2"]);
    Instruction!(AvrState!testChip)[] instrs = [new Ldi!testChip(tok1),
        new Ldi!testChip(tok2), new Rjmp!testChip(tok3)];

    state.setInstructions(instrs);
    auto simstate = sim.run();
    assert(simstate.cycles==4);
    assert(state.registers[1].bytes[0] == 0xff);
    assert(state.registers[2].bytes[0] == 0xaa);
}
