module parser.parser;

import std.file;
import std.regex;
import std.conv;
import std.stdio;
import std.array;
import std.string;
import std.format;
import std.algorithm;
// Input: object dump file, machine instance.
// calls machineinstance.addinstruction(instruction object)


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

  override string toString() {
    return format("line: %x, address: %x, raw: %(%02x %), name: %s, parameters:%(%s, %)",
      this.lineNumber, this.address, this.raw, this.name, this.parameters);
  }
}

struct CodeSection {
 size_t address;
 string name;
}

InstructionToken parseInstruction(in int lineNumber, in string line) {
  auto pieces = split(line, "\t");
  if(pieces.length < 5) {
    return null;
  }
  auto r = ctRegex!(`^\s*([0-9a-f]*):$`);
  auto matches = matchFirst(pieces[0], r);
  if(matches.length < 2) {
    return null;
  }
  ulong address = matches[1].to!ulong(16);
  r = ctRegex!(`^((?:[0-9a-f]{2}\s)*)\s*$`);
  matches = matchFirst(pieces[1], r);
  if(matches.length < 2) {
    return null;
  }
  auto rawStrings = filter!(str => !str.empty)(split(matches[1], " "));
  ubyte[] rawBytes = map!(str => str.to!ubyte(16))(rawStrings).array;
  r = ctRegex!(`^[a-z]*$`);
  matches = matchFirst(pieces[2], r);
  if(matches.empty) {
    return null;
  }
  auto instruction = matches[0];
  r = ctRegex!(`^(?:[0-9a-zA-Z\+]*(:?,\s)?)*$`);
  matches = matchFirst(pieces[3], r);
  if(matches.empty) {
    return null;
  }
  auto parameters = split(matches[0], ", ");
  r = ctRegex!(`; 0x[0-9a-f]* <write_byte>`);
  matches = matchFirst(pieces[4], r);
  if(!matches.empty) {
    instruction = "write_byte";
  }
  return new InstructionToken(lineNumber, address, rawBytes, instruction, parameters);
}

void parse(File file/*, in MachineInstance machineInstance*/) {
  int lineNumber = 0;
  string line;
  while ((line = file.readln()) !is null) {
    InstructionToken token = parseInstruction(lineNumber, line);
    if (token !is null) {
      //machineInstance.addInstruction(token);
      writeln(token);
    }
    lineNumber++;
  }
}

unittest {
  auto tok1 = parseInstruction(123, "      ff:\t0c 94 72 00 \tjmp\t0xe4, Z+\t; 0xe4 <__ctors_end>");
  writeln(tok1);
  auto tok2 = parseInstruction(123, "      ff:\t0c 94 72 00 \tjmp\t0xe4\t; 0xe4 <write_byte>");
  writeln(tok2);
}

void main(string[] args) {
  if (args.length < 2) {
    writeln("usage: parser FILENAME");
    return;
  }

  File file = File(args[1], "r");
  parse(file);
  file.close();
}
