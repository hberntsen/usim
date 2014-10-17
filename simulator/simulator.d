module simulator.simulator;

import machine.state;

struct SimulatorState {
    ulong cycles;
}

//Input: initial machine state (code is part of the machine state)
class Simulator {
    const MachineState machineState;
    SimulatorState simulatorState;

    this(in MachineState initialState) {
        this.machineState = initialState;
        this.simulatorState = SimulatorState(0);
    }

    public SimulatorState run() {
        while (true) {
            // detect end in a better way?
            const cycles = step();
            if (cycles == 0) {
                break;
            }
            this.simulatorState.cycles += cycles;
        }
        return this.simulatorState;
    }

    ulong step() {
        //auto instr = this.machineState.currentInstruction();
        //return this.machineState.apply(instr);
        return 0;
    }
}

