module simulator.simulator;

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

final class Simulator(T) : BatchModeSimulator {
    T machineState;
    SimulatorState simulatorState;

    this(T initialState) {
        this.machineState = initialState;
        this.simulatorState = SimulatorState(0);
    }

    private string handleShowCommand(string[] parameters) {
        if (parameters.length == 0) {
            string[] stringifiedRegisters;
            foreach (idx, register; machineState.valueRegisters) {
                stringifiedRegisters ~= format("r%d: 0x%02x", idx,
                        register.value);
            }
            // todo: abstract this to the machinestate somewhere
            return format(
                    "cycles:\t%d\nregisters:\t%s\n%s\n%s\n",
                    simulatorState.cycles,
                    stringifiedRegisters,
                    machineState.refregs,
                    machineState.currentInstruction,
            );
        }
        return "Nothing to show yet\n";
    }

    public string handleDebugCommand(string command) {
        string[] commands = [
            "run", "s", "step", "continue", "set", "show", "help", "?"
        ];
        auto commandAbbrev = abbrev(commands);

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
                    break;
                case "set":
                    break;
                case "show":
                    return handleShowCommand(parts[1..$]);
                case "help":
                case "?":
                    break;
                default:
                    assert(false);
            }
        } else {
            return "Unknown command: " ~ parts[0] ~ "\n";
        }

        return "OK\n";
    }

    public SimulatorState run() {
        StopWatch stopWatch;
        try {
            stopWatch.start();
            while(step() != step()) {}
            stopWatch.stop();
        } catch (spec.base.EOFException e) {
            stopWatch.stop();
        } catch (Throwable e) {
            stopWatch.stop();
            stderr.writeln("Error");
            stderr.writeln(e);

            stderr.writeln(machineState.currentInstruction());
            debug stderr.writeln(machineState.currentInstruction().token);

            //stderr.writeln(machineState.refregs);
        }
        this.simulatorState.runningTime = stopWatch.peek();
        return this.simulatorState;
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
