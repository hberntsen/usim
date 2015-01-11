#! /usr/bin/env rdmd

import std.stdio;
import std.getopt;
import std.socket;
import std.string;
import std.file;
import std.parallelism;
import std.array;
import std.stream : MemoryStream;
import std.string;

import parser.parser;
import spec.avrchips;
import machine.state;
import machine.factory;
import simulator.simulator;
import spec.avrstate;

void main(string[] args) {
    string filename;
    bool showStatistics = true,
         debugMode = false,
         batchMode = false,
         showHelp = false;
    string machine = "atmega2560";
    char batchInputSeparator = '\n';
    ushort port = 3742;
    size_t batchCount = 1;
    string[string] memFilenames;

    if(args.length < 2) {
        printUsage();
        return;
    }

    getopt(args,
            "help", &showHelp,
            "batch", &batchMode,
            "count", &batchCount,
            "debug", &debugMode,
            "mcu", &machine,
            "port", &port,
            "stats", &showStatistics,
            "memfile", &memFilenames);

    if (showHelp) {
        printUsage();
        return;
    }

    File file;
    try {
        filename = chomp(args[args.length-1]);
        file = File(filename, "r");
    } catch {
        stderr.writeln("File ", filename, " could not be read");
        return;
    }

    if(!(machine in machineFactories)) {
        stderr.writefln("MCU %s not known, valid options: %s", machine,
                machineFactories.keys);
        return;
    }

    ubyte[] data;
    InstructionToken[] instructions = parse(file, data);
    file.close();


    // create and initialize simulator
    auto simulatorFactory = machineFactories[machine];
    auto sim = simulatorFactory.createSimulator(instructions, data);
    readMemFiles(memFilenames, sim.machineState);

    if (debugMode) {
        auto debugSim = cast(DebugSimulator)(sim);
        debugSim.file = filename;
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

                    string response = debugSim.handleDebugCommand(buffer[0 ..  len].dup);
                    input.send(response);
                } while (len != Socket.ERROR && len != 0);
            }
        } catch (Throwable e) {
            writeln(e);
        }
    } else if (batchMode) {
        stderr.writeln("Starting simulation in batch mode");

        char[][] inputs = [];
        string line;
        while((line = stdin.readln(batchInputSeparator)) !is null) {
            line = chomp(line, [batchInputSeparator]);
            inputs ~= cast(char[])(line);
        }

        writeln(inputs);

        if (inputs.length != batchCount) {
            stderr.writefln("Number of inputs (%d) does not match batch count (%d)", inputs.length, batchCount);
            return;
        }

        stderr.writeln("Creating simulators");
        Simulator[] simulators = [];
        for(int i = 0; i < batchCount; ++i) {
            simulators ~= simulatorFactory.createSimulator(instructions, data);
        }

        stderr.writeln("Running simulators");
        foreach (idx, simulator; parallel(simulators)) {
            writeMemFiles(memFilenames, simulator.machineState);
            simulator.machineState.outputBuffer = new MemoryStream(cast(char [])([]));
            simulator.machineState.inputBuffer = new MemoryStream(inputs[idx]);
            simulator.run();
        }

        stderr.writeln("Done");
        foreach (simulator; simulators) {
            debug stderr.writeln("Input:");
            stderr.writeln(cast(string)(simulator.machineState.inputBuffer.data));
            debug stderr.writeln("Output:");
            // FIXME this is not really how you're supposed to read from the
            // stream...
            stdout.writeln(cast(string)(simulator.machineState.outputBuffer.data));
            if (showStatistics) {
                debug stderr.writeln("Stats:");
                stderr.writeln(simulator.state);
            }
        }
    } else {
        writeMemFiles(memFilenames, sim.machineState);
        auto simulatorState = sim.run();
        if(showStatistics) {
            stderr.writeln(simulatorState);
        }
    }
}

void readMemFiles(string[string] filenames, MachineState machineState) {
    foreach (string mem, string filename; filenames) {
        try {
            filename = chomp(filename);
            machineState.memories[mem] = cast(ubyte[])(read(filename));
        } catch {
            stderr.writeln("Memory file ", filename, " could not be read");
        }
    }
}

void writeMemFiles(string[string] filenames, MachineState machineState) {
   foreach(string mem, string filename; filenames) {
       try {
            filename = chomp(filename);
            std.file.write(filename, machineState.memories[mem].data);
       } catch (Exception e) {
            stderr.writeln("Memory file ", filename, " could not be written:");
            stderr.writeln(e.msg);
       }
   }
}

void printUsage() {
    stdout.writeln("Usage: ./usim [OPTIONS] <objdump>");
    stdout.writeln("Options: --batch         Use batch mode [false]");
    stdout.writeln("         --count         Batch mode: number of batches to run [1]");
    stdout.writeln("         --separator     Batch mode: input separator used for dividing input among batches [\\n]");
    stdout.writeln("         --debug         Use debug mode [false]");
    stdout.writeln("         --mcu <mcu>     Select microcontroller [atmega2560]");
    stdout.writefln("                         One of {%-(%s,%)}", machineFactories.keys);
    stdout.writeln("         --port <nr>     Set port for debugger [3742]");
    stdout.writeln("         --stats=false   Show statistics [true]");
    stdout.writeln("         --memfile <memtype=filename> Files that contain initial memory at");
    stdout.writeln("                         start and to which final memory contents are written");
    stdout.writeln("Note that <objdump> is the output of `avr-objdump -D -z <binary>`");
}
