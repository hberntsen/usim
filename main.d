#! /usr/bin/env rdmd

import std.stdio;
import std.getopt;

import parser.parser;
import spec.atmega2560;
import spec.base;
import simulator.simulator;

void main(string[] args) {
    string filename;
    bool nostats = false;
    getopt(args,
            "file", &filename,
            "nostats", &nostats);

    File file = File(filename, "r");
    InstructionToken[] instructions = parse(file);
    file.close();

    auto state = new AtMega2560State;
    auto sim = new Simulator!AtMega2560State(state);

    sim.initialiseInstructions(instructions);
    auto simulatorState = sim.run();
    if(!nostats) {
        stderr.writeln(simulatorState);
    }
}
