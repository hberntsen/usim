module simulator.simulator;

import std.stdio;
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

//Input: initial machine state (code is part of the machine state)
class Simulator(T) {
    T machineState;
    SimulatorState simulatorState;

    this(T initialState) {
        this.machineState = initialState;
        this.simulatorState = SimulatorState(0);
    }

    public SimulatorState run() {
        StopWatch stopWatch;
        size_t previousPc;
        try {
            stopWatch.start();
            step();
            do {
                previousPc = machineState.programCounter;
                step();
            } while(previousPc != machineState.programCounter);
            stopWatch.stop();
        } catch (spec.base.EOFException e) {
            stopWatch.stop();
        } catch (Throwable e) {
            stopWatch.stop();
            stderr.writeln("Error");
            stderr.writeln(e);

            stderr.writeln(machineState.currentInstruction());
            stderr.writeln(machineState.currentInstruction().token);

            stderr.writeln(machineState.refregs);
        }
        this.simulatorState.runningTime = stopWatch.peek();
        return this.simulatorState;
    }

    ulong step() {
        writefln("\tPC (before): %x", machineState.programCounter);
        auto instr = this.machineState.fetchInstruction();
        writefln("\tToken: %s", instr.token);
        writefln("\tRefrefs (before): %s", machineState.refregs);
        writefln("\tRegisters (before): %s", machineState.registers);
        writefln("\tFlags (before): %s", machineState.sreg);
        writefln("\tSP (before): %s", machineState.stackPointer);
        writefln("\tStack (before): [%(%x, %)]",
                machineState.data[machineState.stackPointer+1 .. $]);
        writef("Applying instruction '%s'", instr.name);
        const ulong cycles = instr.callback(machineState);
        writefln(" - DONE (%d cycles)", cycles);
        //writefln("\tSP (after): %s", machineState.stackPointer);
        //writefln("\tRefrefs (after): %s", machineState.refregs);
        //writefln("\tRegisters (after): %s", machineState.registers);
        //writefln("\tFlags (after): %s", machineState.sreg);

        this.simulatorState.cycles += cycles;
        return cycles;
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
    Instruction!(AvrState)[] instrs = [new Ldi(tok1), new Ldi(tok2), new Rjmp(tok3)];

    state.setInstructions(instrs);
    auto simstate = sim.run();
    assert(simstate.cycles==4);
    assert(state.registers[1].bytes[0] == 0xff);
    assert(state.registers[2].bytes[0] == 0xaa);
}
