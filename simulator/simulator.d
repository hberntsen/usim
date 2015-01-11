module simulator.simulator;

import std.algorithm;
import std.stdio;
import std.array;
import std.regex;
import std.datetime : StopWatch, TickDuration;
import machine.state;
import spec.base;
import parser.parser;
import std.string;
import std.conv;
import simulator.debugger;

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

class DebuggerState {
    string[size_t] breakpoints;
    private ushort nextId = 0;
    ulong[size_t] since;
    string[string] configuration;

    this() {
        configuration = [
            "memory_width": "2",
            "help_compact": "false"
        ];
    }

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

    public void registerSince(size_t line, SimulatorState state) {
        since[line] = state.cycles;
    }

    public bool unsetBreakpoint(size_t line) {
        return breakpoints.remove(line);
    }

    public string[] getBreakpoints() {
        string[] repr;
        foreach (idx, key; breakpoints.keys.sort) {
            repr ~= format("%02d. %s (%d)", idx + 1, breakpoints[key], key);
        }

        return repr;
    }
}

interface Simulator {
    @property MachineState machineState();
    @property MachineState machineState(MachineState newState);
    @property SimulatorState state();

    SimulatorState run(bool withBreakpoints = false);
    size_t step();
}

abstract class DebugSimulator : Simulator {
    SimulatorState simulatorState;
    DebuggerState debuggerState;

    private string file_;

    @property string file() { return file_;}
    @property string file(string newFile) { file_=newFile; return newFile;}

    @property SimulatorState state() { return simulatorState; }

    string handleDebugCommand(string command);

    public SimulatorState continueUntilBreakpoint() {
        return run(true);
    }

    public InstructionI getInstruction();

    //public SimulatorState run(bool withBreakpoints = false);
}

//Input: initial machine state (code is part of the machine state)
final class AvrSimulator(T) : DebugSimulator {
    private T machineState_;

    @property MachineState machineState() {
        return machineState_;
    }
    @property MachineState machineState(MachineState newState) {
        machineState_ = cast(T)(newState);
        return machineState_;
    }

    this(T initialState) {
        debug writeln("Simulator initialized");
        this.machineState_ = initialState;
        this.simulatorState = SimulatorState(0);
        this.debuggerState = new DebuggerState();
    }

    override InstructionI getInstruction() {
        return machineState_.nextInstruction();
    }

    override public string handleDebugCommand(string command) {
        DebugCommand rootCommand = new EmptyCommand!T();
        string[] input = split(command);
        return rootCommand.run(this, input);
    }

    public SimulatorState run(bool withBreakpoints = false) {
        StopWatch stopWatch;
        try {
            stopWatch.start();
            size_t lastAddress, currentAddress = machineState_.nextInstruction.address;
            do {
                if (withBreakpoints &&
                        debuggerState.hasBreakpoint(
                            machineState_.nextInstruction.token.lineNumber)) {
                    debuggerState.registerSince(
                            machineState_.nextInstruction.token.lineNumber,
                            simulatorState
                    );
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

    size_t step() {
        auto instr = this.machineState_.fetchInstruction();
        const ulong cycles = instr.callback(machineState_);
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
