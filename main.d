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
            "mcu", &machine,
            "nostats", &nostats);

    File file;
    try {
        file = File(filename, "r");
    }
    catch {
        stderr.writeln("File ", filename, " could not be read");
        return;
    }
    ubyte[] data;
    InstructionToken[] instructions = parse(file, data);
    file.close();

    auto factory = machineFactories[machine];
    auto sim = factory.createBatchModeSimulator(instructions,data);

    auto simulatorState = sim.run();
    if(!nostats) {
        stderr.writeln(simulatorState);
    }
}
