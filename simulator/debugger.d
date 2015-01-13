module simulator.debugger;

import simulator.simulator;
import spec.avrchips;
import spec.avrstate;
import std.string;
import std.conv;
import std.array;
import std.stdio;


struct CommandParameter {
    string name;
    bool optional;

    this(string name, bool optional = false) {
        this.name = name;
        this.optional = optional;
    }
}

abstract class DebugCommand {
    bool available = true;
    string[] commands;
    CommandParameter[] parameters = [];
    string description = "No description set";

    DebugCommand[] subCommands;

    string run(DebugSimulator, string[]);

    DebugCommand selectSubcommand(string input) {
        string[] commands;
        DebugCommand[string] lookup;
        foreach (cmd; subCommands) {
            commands ~= cmd.commands;
            foreach (c; cmd.commands) {
                lookup[c] = cmd;
            }
        }
        auto abbr = abbrev(commands);
        if (input in abbr) {
            return lookup[abbr[input]];
        }
        return null;
    }

    protected string delegateToSubCommand(DebugSimulator sim, string[] input) {
        if (input.length == 0) {
            return "No command specified\n";
        } else {
            auto cmd = selectSubcommand(input[0]);
            if (cmd !is null && cmd.available) {
                return cmd.run(sim, input[1 .. $]);
            } else {
                return "Unknown command\n";
            }
        }
    }

    protected string parameterString() {
        string output[];
        foreach (param; parameters) {
            if (param.optional) {
                output ~= format("[%s]", param.name);
            } else {
                output ~= param.name;
            }
        }
        return join(output, " ");
    }

    protected string help(bool compact = false) {
        string command = join(commands, "|");
        string params;
        if (parameters.empty && !compact) {
            params = "-";
        } else {
            params = parameterString();
        }

        if (compact) {
            return format("%s %s", command, params);
        } else {
            return format("%-20s %-20s %s", command, params, description);
        }
    }
}

class BreakPointSetCommand : DebugCommand {
    this() {
        commands = ["breakpoint"];
        description = "set a breakpoint";
        parameters = [
            CommandParameter("linenumber"),
            CommandParameter("name", true)
        ];
    }

    override string run(DebugSimulator sim, string[] input) {
        if (input.length < 1) {
            return "Usage: " ~ help() ~ "\n";
        }
        string line = input[0];
        string name = null;
        if (input.length == 2) {
            name = input[1];
        }
        size_t breakpoint = to!size_t(line);
        sim.debuggerState.setBreakpoint(breakpoint, name);
        return format("Breakpoint set at line %d\n", breakpoint);
    }
}

class BreakPointShowCommand : DebugCommand {
    this() {
        commands = ["breakpoints"];
        description = "show breakpoints";
    }

    override string run(DebugSimulator sim, string[] input) {
        auto brs = sim.debuggerState.getBreakpoints();
        if (brs.empty) {
            return "no breakpoints set\n";
        }
        return join(brs, "\n") ~ "\n";
    }
}

class BreakPointUnsetCommand : DebugCommand {
    this() {
        commands = ["breakpoint"];
        description = "unset a breakpoint";
        parameters = [
            CommandParameter("linenumber"),
        ];
    }

    override string run(DebugSimulator sim, string[] input) {
        if (input.length < 1) {
            return "Usage: " ~ help() ~ "\n";
        }
        size_t breakpoint = to!size_t(input[0]);
        sim.debuggerState.unsetBreakpoint(breakpoint);
        return format("breakpoint removed at line %d\n", breakpoint);
    }
}

class SinceCommand : DebugCommand {
    this() {
        commands = ["since"];
        description = "show cycle count difference from breakpoints";
    }

    override string run(DebugSimulator sim, string[] input) {
        auto simState = sim.simulatorState;
        auto cyclesNow = simState.cycles;
        auto brs = sim.debuggerState.breakpoints;

        string[] output = ["cycles since:"];
        if (brs is null) {
            output ~= "no breakpoints set";
        } else {
            foreach (line, name; brs) {
                string diff;
                if (line in sim.debuggerState.since) {
                    diff = format("%d cycles", cyclesNow -
                            sim.debuggerState.since[line]);
                } else {
                    diff = "-";
                }
                output ~= format("%s: %s", name, diff);
            }
        }
        return join(output, "\n") ~ "\n";
    }
}

class MemoryCommand(T) : DebugCommand {
    private string memory;

    this(string memory) {
        parameters = [
            CommandParameter("first", false),
            CommandParameter("last", true)
        ];
        this.memory = memory;
    }

    private size_t parseOffset(string input) {
        if (input.length >= 3 && input[0 .. 2] == "0x") {
            string numeric = input[2 .. $];
            return parse!size_t(numeric, 16);
        } else if (isNumeric(input)) {
            return to!size_t(input);
        } else {
            return -1;
        }
    }

    override string run(DebugSimulator sim, string[] input) {
        auto state = cast(T)(sim.machineState);

        if (input.length < 1) {
            return "Usage: " ~ help() ~ "\n";
        }

        size_t begin, end;

        if (input.length >= 1) {
            begin = parseOffset(input[0]);
            if (begin == -1) {
                return format("'first' (%s) is not a valid offset in either integer of hexadecimal representation\n");
            }
        }

        if (input.length == 1) {
            end = begin;
        } else {
            end = parseOffset(input[1]);
            if (end == -1) {
                return format("'last' (%s) is not a valid offset in either integer of hexadecimal representation\n", input[1]);
            }
        }

        if (begin > end) {
            return "ERROR: first > last\n";
        }

        auto mem = state.memories()[memory];

        string[] dataSlice;
        auto slice = mem[begin .. end + 1];
        size_t width = parseOffset(sim.debuggerState.configuration["memory_width"]);
        for(size_t idx = 0; begin + idx <= end; idx += width) {
            auto elements = slice[idx .. idx + width];
            dataSlice ~= format("0x%06x 0x%(%02x%)", idx + begin, elements);
        }
        return join(dataSlice, "\n") ~ "\n";
    }
}

class DataCommand(T) : MemoryCommand!T {
    this() {
        commands = ["data"];
        description = "show a given selection of the data memory";
        super("data");
    }
}

class ProgramCommand(T) : MemoryCommand!T {
    this() {
        commands = ["program"];
        description = "show a given selection of the data memory";
        super("data");
    }
}

class EEPROMCommand(T) : MemoryCommand!T {
    this() {
        commands = ["eeprom"];
        description = "show a given selection of the data memory";
        super("data");
    }
}

class FlagsCommand(T) : DebugCommand {
    this() {
        commands = ["flags"];
        description = "show status flags";
    }

    override string run(DebugSimulator sim, string[] input) {
        auto state = cast(T)(sim.machineState);
        auto flags = state.sreg;
        ushort[string] repr = [
            "I": flags.I() ? 1 : 0,
            "T": flags.T() ? 1 : 0,
            "H": flags.H() ? 1 : 0,
            "S": flags.S() ? 1 : 0,
            "V": flags.V() ? 1 : 0,
            "N": flags.N() ? 1 : 0,
            "Z": flags.Z() ? 1 : 0,
            "Z": flags.Z() ? 1 : 0,
        ];

        string[] flagRepr;
        foreach (idx, key; repr.keys) {
            flagRepr ~= format("%s: %d", key, repr[key]);
        }
        return join(flagRepr, "\n") ~ "\n";
    }

}

class XyzCommand(T) : DebugCommand {
    this() {
        commands = ["xyz", "refregs"];
        description = "show the X, Y and Z registers";
    }

    override string run(DebugSimulator sim, string[] input) {
        auto state = cast(T)(sim.machineState);
        auto xyz = state.refregs;
        string output[];
        foreach (refreg; xyz) {
            output ~= format("%s: %s", refreg.name, refreg);
        }
        return join(output, "\n") ~ "\n";
    }
}

class StackCommand(T) : DebugCommand {
    this() {
        commands = ["stack"];
        description = "show the stack contents";
    }

    override string run(DebugSimulator sim, string[] input) {
        auto state = cast(T)(sim.machineState);
        auto sp = state.stackPointer;

        if (sp.value  + 1 > state.data.length) {
            return format("Error when reading stack, SP: %s", sp);
        }

        ubyte[] stack = state.data[sp.value + 1 .. $];
        string[] stackRepr;
        foreach (idx, el; stack) {
            stackRepr ~= format("0x%06x 0x%02x", sp + idx, el);
        }

        return join(stackRepr, "\n") ~ "\n";
    }
}

class IORegisterCommand(T) : DebugCommand {
    this() {
        commands = ["io"];
        description = "show the io registers";
    }

    override string run(DebugSimulator sim, string[] input) {
        auto state = cast(T)(sim.machineState);
        string[] dataSlice;
        foreach (idx, el; state.ioRegisters) {
            auto addr = cast(ushort)(idx + 0x20);
            dataSlice ~= format("0x%x %s", addr, el);
        }
        return join(dataSlice, "\n") ~ "\n";
    }
}

class StateCommand(T) : DebugCommand {
    this() {
        commands = ["state", "stats"];
        description = "show the current state of the simulation";
    }

    override string run(DebugSimulator sim, string[] input) {
        auto simState = sim.state;
        string[] output = [
            format("total cycles: %d", simState.cycles),
        ];
        return join(output, "\n") ~ "\n";
    }
}

class ConfigurationSetCommand : DebugCommand {
    this() {
        commands = ["cfg", "configuration"];
        description = "set a configuration option, see `show configuration` for a list";
    }

    override string run(DebugSimulator sim, string[] input) {
        if (input.length < 2) {
            return "Usage: " ~ help() ~ "\n";
        }

        string key = input[0];
        string value = input[1];

        if (key in sim.debuggerState.configuration) {
            sim.debuggerState.configuration[key] = value;
            return format("Set option '%s' to '%s'\n", key, value);
        } else {
            return "Unknown configuration option\n";
        }
    }
}

class ConfigurationShowCommand : DebugCommand {
    this() {
        commands = ["cfg", "configuration"];
        description = "show a specific or all configuration option(s)";
    }

    override string run(DebugSimulator sim, string[] input) {
        auto cfg = sim.debuggerState.configuration;

        string[] keys;
        if (input.length == 0) {
            keys = cfg.keys.sort;
        } else {
            keys = input;
        }

        string[] output;
        foreach (key; cfg.keys.sort) {
            output ~= format("%-10s %s", key, cfg[key]);
        }
        return join(output, "\n") ~ "\n";
    }
}

class InstructionShowCommand: DebugCommand {
    this() {
        commands = ["instruction"];
        description = "show the current instruction slated for execution";
    }

    override string run(DebugSimulator sim, string[] input) {
        return format("%s\n", sim.getInstruction());
    }
}

class RegisterShowCommand(T) : DebugCommand {
    this() {
        commands = ["registers"];
        description = "show register content for the given registers, or all if none are specified";
        parameters = [
            CommandParameter("registers", true)
        ];
    }

    override string run(DebugSimulator sim, string[] input) {
        string[] registers;
        auto state = cast(T)(sim.machineState);
        foreach (idx, register; state.registers) {
            registers ~= format("%-6s %s", register.name, register);
        }

        if (input.empty) {
            return join(registers, "\n") ~ "\n";
        }


        string[] registerSet;
        foreach (idx, param; input) {
            size_t specifier;
            if (param[0] == 'r') {
                specifier = to!size_t(param[1 .. $]);
            } else {
                specifier = to!size_t(param[0 .. $]);
            }
            registerSet ~= registers[specifier];
        }
        return join(registerSet, "\n") ~ "\n";
    }
}

class FileCommand : DebugCommand {
    this() {
        commands = ["file"];
        description = "print path to the input file";
    }

    override string run(DebugSimulator sim, string[] input) {
        return sim.file ~ "\n";
    }
}

class RunCommand : DebugCommand {
    this() {
        commands = ["run"];
        description = "run simulation until the end";
    }

    override string run(DebugSimulator sim, string[] input) {
        sim.run(false);
        return "OK\n";
    }
}

class ContinueCommand : DebugCommand {
    this() {
        commands = ["continue"];
        description = "continue until the next breakpoint";
    }

    override string run(DebugSimulator sim, string[] input) {
        sim.run(true);
        return "OK\n";
    }
}

class StepCommand : DebugCommand {
    this() {
        commands = ["s", "step"];
        description = "run a single instruction";
    }

    override string run(DebugSimulator sim, string[] input) {
        sim.step();
        return "OK\n";
    }
}

class ShowCommand(T) : DebugCommand {
    this() {
        commands = ["show"];
        description = "show information - see `show help`";
        subCommands = [
            new BreakPointShowCommand(),
            new ConfigurationShowCommand(),
            new DataCommand!T(),
            new EEPROMCommand!T(),
            new FileCommand(),
            new FlagsCommand!T(),
            new HelpCommand(this),
            new IORegisterCommand!T(),
            new InstructionShowCommand(),
            new ProgramCommand!T(),
            new RegisterShowCommand!T(),
            new SinceCommand(),
            new StackCommand!T(),
            new StateCommand!T(),
            new XyzCommand!T()
        ];
    }

    override string run(DebugSimulator sim, string[] input) {
        return delegateToSubCommand(sim, input);
    }
}

class SetCommand(T) : DebugCommand {
    this() {
        commands = ["set"];
        description = "set configuration, breakpoints, etc.";
        subCommands = [
            new BreakPointSetCommand(),
            new ConfigurationSetCommand(),
            new HelpCommand(this)
        ];
    }

    override string run(DebugSimulator sim, string[] input) {
        return delegateToSubCommand(sim, input);
    }
}


class UnsetCommand(T) : DebugCommand {
    this() {
        commands = ["unset"];
        description = "unset breakpoints, etc. - see `unset help`";
        subCommands = [
            new BreakPointUnsetCommand(),
            new HelpCommand(this)
        ];
    }

    override string run(DebugSimulator sim, string[] input) {
        return delegateToSubCommand(sim, input);
    }
}

class HelpCommand : DebugCommand {
    DebugCommand parent;

    this(DebugCommand parent) {
        commands = ["?", "help"];
        description = "show help for the given command";
        parameters = [
            CommandParameter("compact", true),
            CommandParameter("recursive", true)
        ];
        this.parent = parent;
    }

    override string run(DebugSimulator sim, string[] input) {
        //string[string] commands;
        bool compact = false, recursive = false;
        if (!input.empty) {
            compact = input[0] == "yes" || input[0] == "true" || input[0] == "1";
            if (input.length >= 2) {
                recursive = input[1] == "yes" || input[1] == "true" || input[1] == "1";
            }
        } else {
            string config = sim.debuggerState.configuration["help_compact"];
            compact = config == "yes" || config == "true" || config == "1";
        }

        string[] output;
        if (compact) {
            output ~= "commands (command <parameters>):";
        } else {
            output ~= format("%-20s %-20s %s", "command", "parameters", "description");
            output ~= "-".replicate(output[0].length);
        }
        foreach (cmd; parent.subCommands) {
            if (cmd.available) {
                output ~= "• " ~ cmd.help(compact);
                // TODO actually make recursive -_-
                if (recursive) {
                    foreach (sub; cmd.subCommands) {
                        output ~=  "\t• "  ~ sub.help(compact);
                    }
                }
            }
        }

        return join(output, "\n") ~ "\n";
    }
}

class EmptyCommand(T) : DebugCommand {
    this() {
        commands = [];
        subCommands = [
            new ShowCommand!T(),
            new SetCommand!T(),
            new UnsetCommand!T(),
            new StepCommand(),
            new ContinueCommand(),
            new RunCommand(),
            new HelpCommand(this)
        ];
    }

    override string run(DebugSimulator sim, string[] input) {
        return delegateToSubCommand(sim, input);
    }
}
