#! /usr/bin/env rdmd
import std.stdio;

import parser.parser;
import spec.atmega2560;
import spec.base;
import simulator.simulator;

void main() {
    File file = File("tests/test_write/test_write_atmega2560.dump3", "r");
    InstructionToken[] instructions = parse(file);
    file.close();

    auto state = new AtMega2560State;
    auto sim = new Simulator!AtMega2560State(state);

    sim.initialiseInstructions(instructions);
    sim.run();
}
