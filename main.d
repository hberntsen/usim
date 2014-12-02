#! /usr/bin/env rdmd

import std.stdio;
import std.getopt;
import std.socket;
import std.string;

import parser.parser;
import spec.atmega2560;
import machine.state;
import simulator.simulator;
import spec.avrstate;

void main(string[] args) {
    string filename;
    bool showStatistics = true,
         debugMode = false,
         batchMode = false;
    string machine = "atmega2560";
    ushort port = 3742;

    getopt(args,
            "batch", &batchMode,
            "debug", &debugMode,
            "file", &filename,
            "mcu", &machine,
            "port", &port,
            "stats", &showStatistics);

    File file;
    try {
        filename = chomp(filename);
        file = File(filename, "r");
    } catch {
        stderr.writeln("File ", filename, " could not be read");
        return;
    }

    ubyte[] data;
    InstructionToken[] instructions = parse(file, data);
    file.close();

    auto simulatorFactory = machineFactories[machine];
    auto sim = simulatorFactory.createBatchModeSimulator(instructions, data);

    if (debugMode) {
        auto avrSim = cast(Simulator!AvrState)(sim);
        avrSim.file = filename;
        debug writefln("Opening a socket for the debugger", port);
        Socket s = new TcpSocket;
        scope(exit) {
            debug writeln("Closing socket");
            s.close();
        }
        try {
            s.blocking = true;
            s.bind(new InternetAddress(port));

            s.listen(10);
            debug writefln("Listening on port %d", port);

            while (true) {
                Socket input = s.accept();
                scope(exit) {
                    debug writeln("Closing input socket");
                    input.close();
                }
                debug stderr.writeln("accepted connection");
                char[64] buffer;
                ptrdiff_t len;
                do {
                    len = input.receive(buffer);

                    if (len == Socket.ERROR) {
                        stderr.writeln("Error: " ~ input.getErrorText());
                        break;
                    }

                    if (len == 0) {
                        debug stderr.writeln("Input socket closed");
                        break;
                    }

                    debug stderr.writefln("%s", buffer[0 .. len]);

                    string response = avrSim.handleDebugCommand(buffer[0 ..  len].dup);
                    input.send(response);
                } while (len != Socket.ERROR && len != 0);
            }
        } catch (Throwable e) {
            writeln(e);
        }
    } else if (batchMode) {
    } else {
        auto simulatorState = sim.run();
        if(showStatistics) {
            stderr.writeln(simulatorState);
        }
    }
}
