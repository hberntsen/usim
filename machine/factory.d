module machine.factory;

import machine.state : MachineState;
import simulator.simulator;
import parser.parser : InstructionToken;

abstract class MachineFactory {
    public MachineState createState(in InstructionToken[] token, in ubyte[] data) const;
    public BatchModeSimulator createBatchModeSimulator(in InstructionToken[] token, in ubyte[] data) const;
}

MachineFactory[string] machineFactories;
