module simulator.simulator;

import std.stdio;
import std.datetime : StopWatch, TickDuration;
import machine.state;
import spec.base;
import spec.atmega2560;
import parser.parser;

struct SimulatorState {
    ulong cycles;
    TickDuration runningTime;
}

//Input: initial machine state (code is part of the machine state)
class Simulator(T) {
    T machineState;
    SimulatorState simulatorState;

    this(T initialState) {
        this.machineState = initialState;
        this.simulatorState = SimulatorState(0);
    }

    void initialiseInstructions(InstructionToken[] tokens) {
        Instruction!T[] instructions = [];
        foreach (i, tok; tokens) {
            instructions ~= createInstruction!T(tok);
        }
        machineState.setInstructions(instructions);
    }

    Instruction!T createInstruction(AtMega2560State)(InstructionToken tok) {
        //writeln(tok);
        switch (tok.name) {
            case "brne": return new Brne(tok);
            case "call": return new Call(tok);
            case "cli": return new Cli(tok);
            case "cpc": return new Cpc(tok);
            case "cpi": return new Cpi(tok);
            case "cpse": return new Cpse(tok);
            case "eor": return new Eor(tok);
            case "ldi": return new Ldi(tok);
            case "lds": return new Lds(tok);
            case "out": return new Out(tok);
            case "st": return new St(tok);
            case "sts": return new Sts(tok);
            case "nop": return new Nop(tok);
            case "ret": return new Ret(tok);
            case "rjmp": return new Rjmp(tok);
            case "jmp": return new Jmp(tok);
            case "sbrs": return new Sbrs(tok);
            case "write_byte": return new WriteByte(tok);
            default: throw new Exception("Unknown instruction: " ~ tok.name);
        }
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
        }
        this.simulatorState.runningTime = stopWatch.peek();
        return this.simulatorState;
    }

    ulong step() {
        auto instr = this.machineState.fetchInstruction();
        //writefln("Applying instruction '%s'", instr.name);
        //writefln("\tToken: %s", instr.token);
        //writefln("\tRefrefs (before): %s", machineState.refregs);
        //writefln("\tRegisters (before): %s", machineState.registers);
        //writefln("\tFlags (before): %s", machineState.sreg);
        //writefln("\tSP (before): %s", machineState.stackPointer);
        //write("\tApplying callback");
        const ulong cycles = instr.callback(machineState);
        //writefln(" - DONE (%d cycles)", cycles);
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

    auto state = new AtMega2560State;
    auto sim = new Simulator!AtMega2560State(state);

    InstructionToken tok1 = new InstructionToken(1, 0x00, [0xcf, 0xef], "ldi",
            ["r1", "0xFF"]);
    InstructionToken tok2 = new InstructionToken(2, 0x02, [0xcf, 0xef], "ldi",
            ["r2", "0xAA"]);
    InstructionToken tok3 = new InstructionToken(3, 0x04, [0x02, 0xc0], "rjmp",
            [".-2"]);
    Instruction!(AtMega2560State)[] instrs = [new Ldi(tok1), new Ldi(tok2), new Rjmp(tok3)];

    state.setInstructions(instrs);
    auto simstate = sim.run();
    assert(simstate.cycles==4);
    assert(state.registers[1].bytes[0] == 0xff);
    assert(state.registers[2].bytes[0] == 0xaa);
}
