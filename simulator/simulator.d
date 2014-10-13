module simulator.simulator;

import std.stdio;

import machine.state;
import spec.base;

struct SimulatorState {
    ulong cycles;
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
        try {
            while (true) {
                step();
            }
        } catch (spec.base.EOFException e) {
        }
        return this.simulatorState;
    }

    ulong step() {
        auto instr = this.machineState.fetchInstruction();
        writefln("Applying instruction '%s'", instr.name);
        write("\tApplying callback");
        const ulong cycles = instr.callback(machineState);
        writefln(" - DONE (%d cycles)", cycles);
        this.simulatorState.cycles += cycles;
        return cycles;
    }
}

unittest {
    writeln("simulator");
    import spec.base;
    import spec.atmega2560;
    import parser.parser;

    auto state = new AtMega2560State;
    auto sim = new Simulator!AtMega2560State(state);

    InstructionToken tok1 = new InstructionToken(1, 0x00, [0xcf, 0xef], "ldi",
            ["r1", "0xFF"]);
    InstructionToken tok2 = new InstructionToken(2, 0x02, [0xcf, 0xef], "ldi",
            ["r2", "0xFF"]);
    InstructionToken tok3 = new InstructionToken(3, 0x04, [0x02, 0xc0], "rjmp",
            [".-4"]);
    Instruction!(AtMega2560State)[] instrs = [new Ldi(tok1), new Ldi(tok2)/*, new Rjmp(tok3)*/];

    writeln(instrs);
    state.setInstructions(instrs);

    writeln("Applying instructions");
    sim.run();

    writeln(sim.machineState.registers);
}
