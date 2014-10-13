module parser.parser;

import std.file;
import std.regex;
import std.conv;
import std.stdio;
import std.array;
import std.string;
import std.format;
import std.algorithm;

class InstructionToken {
    size_t lineNumber;
    size_t address;
    ubyte[] raw;
    string name;
    string[] parameters;
    this (size_t lineNumber, size_t address, ubyte[] raw, string name, string[]
            parameters)
    {
        this.lineNumber = lineNumber;
        this.address = address;
        this.raw = raw;
        this.name = name;
        this.parameters = parameters;
    }

    override string toString() const {
        return format("line: %4d, address: %4x, raw: %(%02x %), name: %s, parameters:%(%s, %)",
                this.lineNumber, this.address, this.raw, this.name, this.parameters);
    }
}

InstructionToken parseInstruction(in int lineNumber, in string line) {
    string[] pieces = split(line, "\t");
    for (size_t i = 0; i < pieces.length; ++i) {
        pieces[i] = strip(chomp(pieces[i]));
    }

    auto r = ctRegex!(`^\s*([0-9a-f]*):\s*$`);
    auto matches = matchFirst(pieces[0], r);
    if(matches.length < 2) {
        return null;
    }
    size_t address = matches[1].to!size_t(16);

    r = ctRegex!(`^((?:[0-9a-f]{2})(?:\s*[0-9a-f]{2})*)$`);
    matches = matchFirst(pieces[1], r);
    if(matches.length < 2) {
        writeln("No raw");
        return null;
    }
    auto rawStrings = filter!(str => !str.empty)(split(matches[1], " "));
    ubyte[] rawBytes = map!(str => str.to!ubyte(16))(rawStrings).array;

    r = ctRegex!(`^\s*([a-z]+)\s*$`);
    matches = matchFirst(pieces[2], r);
    if(matches.length < 2) {
        return null;
    }
    auto instruction = matches[1];

    string parameters[] = [];
    if (pieces.length >= 4) {
        r = ctRegex!(`^\s*(.+)\s*$`);
        matches = matchFirst(pieces[3], r);
        if(matches.empty) {
            writeln("Invalid parameters");
            return null;
        }
        parameters = split(matches[0], ", ");
    }
    if (pieces.length == 5) {
        r = ctRegex!(`;\s*0x[0-9a-f]* <write_byte>`);
        matches = matchFirst(pieces[4], r);
        if(!matches.empty) {
            instruction = "write_byte";
        }
    }
    return new InstructionToken(lineNumber, address, rawBytes, instruction, parameters);
}

InstructionToken[] parse(File file) {
    InstructionToken[] instructions;
    instructions.length = 100;
    int lineNumber = 0;
    int i = 0;
    string line;
    while ((line = file.readln()) !is null) {
        InstructionToken token = parseInstruction(lineNumber, line);
        if (token !is null) {
            if (i == instructions.length) {
                instructions.length *= 2;
            }
            instructions[i++] = token;
        }
        lineNumber++;
    }
    instructions.length = i;
    return instructions;
}

unittest {
    InstructionToken[] tokens = [
        parseInstruction(123, "      ff:\t0c 94 72 00 \tjmp\t0xe4, Z+\t; 0xe4 <__ctors_end>"),
        parseInstruction(123, "      ff:\t0c 94 72 00 \tjmp\t0xe4\t; 0xe4 <write_byte>"),
        parseInstruction(123, " 160:\t  08 95       \t ret"),
        parseInstruction(123, " 19a2:\t ff cf       \trjmp\t.-2      \t; 0x19a2 <__stop_program>")
    ];
    foreach (i, tok; tokens) {
        writeln(tok);
    }
}

unittest {
    //File file = File("tests/test_write/test_write_atmega2560.dump", "r");
    //InstructionToken[] instructions = parse(file);
    //file.close();
    //foreach (i, instr; instructions) {
        //writefln("%4d: %s", i, instr);
    //}
}
