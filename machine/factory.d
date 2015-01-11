module machine.factory;

import machine.state : MachineState;
import simulator.simulator : Simulator;
import parser.parser : InstructionToken;

abstract class MachineFactory {
    public MachineState createState(in InstructionToken[] token, in ubyte[] data) const;
    public Simulator createSimulator(in InstructionToken[] token, in ubyte[] data) const;
}

MachineFactory[string] machineFactories;
