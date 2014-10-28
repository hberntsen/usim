#! /usr/bin/env rdmd

import std.stdio;
import std.getopt;

import parser.parser;
import spec.atmega2560;
import machine.state;
import simulator.simulator;

void main(string[] args) {
    string filename;
    bool nostats = false;
    string machine = "atmega2560";
    getopt(args,
            "file", &filename,
            "nostats", &nostats);

    File file = File(filename, "r");
    InstructionToken[] instructions = parse(file);
    file.close();

    auto factory = machineFactories[machine];
    AtMega2560State state = cast(AtMega2560State)(factory.createState(instructions));

    auto sim = new Simulator!AtMega2560State(state);

    auto simulatorState = sim.run();
    if(!nostats) {
        stderr.writeln(simulatorState);
    }
}
