module parser.parser;

import std.file;
import std.regex;
import std.conv;
import std.stdio;
import std.array;
import std.string;
// Input: object dump file, machine instance. 
// calls machineinstance.addinstruction(instruction object)


struct InstructionToken {
  ulong lineNumber;
  ulong address;
  byte[] raw;
  string name;
  string[] parameters; 
}

struct CodeSection {
 ulong address;
 string name;
}

InstructionToken parseInstruction(in int lineNumber, in string line) {
  auto pieces = split(line, "\t");
  foreach(piece; pieces) {
    writefln("'%s'", piece);
  }
  auto r = ctRegex!(`^\s*([0-9a-]*):$`);
  auto matches = matchFirst(pieces[0], r);
  if(matches.length < 2) {
    assert(false, "No match on address");
    //TODO: not a valid instruction
  }
  auto address = matches[1];//TODO: convert
  r = ctRegex!(`^((?:[0-9a-f]{2}\s)*)\s*$`);
  matches = matchFirst(pieces[1], r);
  if(matches.length < 2) {
    assert(false, "No match on raw");
    //TODO: not a valid instruction
  }
  auto rawStrings = split(matches[1], " "); //TODO: convert
  r = ctRegex!(`^[a-z]*$`);
  matches = matchFirst(pieces[2], r);
  if(matches.empty) {
    assert(false, "No match on instruction");
    //TODO: not a valid instruction
  }
  auto instruction = matches[0];
  r = ctRegex!(`^(?:[0-9a-zA-Z\+]*(:?,\s)?)*$`);
  matches = matchFirst(pieces[3], r);
  if(matches.empty) {
    assert(false, "No match on paramaters");
    //TODO: not a valid instruction
  }
  auto parameters = split(matches[0], ", ");
  return InstructionToken (0, 0, new byte[0], "", new string[0]); //TODO: use retreived values
}

InstructionToken parseLine(in int lineNumber, in string line) {
  auto r = ctRegex!(`\s*([0-9a-f]*):\s((?:[0-9a-f]*\s)*)\s*([a-z]*)\s((?:[a-zA-Z0-9\+]*,?\s*)*)\s*;`);
  auto matches =  matchFirst(line, r);
  assert(!matches.empty);
  writeln(matches);
  writeln(matches.length());
  foreach(mat; matches) {
    writeln(mat);
  }
  InstructionToken token = InstructionToken (
    lineNumber,
    0xa8, //todo retrieve from matches[1]
    new byte[0], //todo retrieve from matches[2]
    matches[3],
    new string[0] //todo: retrieve from matches[3]
  );
  return token;
}

unittest {
  auto tok = parseLine(123,"      a8:\t83 81        \tldd\tr24, Z+3; 0x03");
  assert(tok.lineNumber == 123);
  assert(tok.address == 0xa8);
  assert(tok.name == "ldd");
  
  auto tok2 = parseLine(123, "       0:\t0c 94 72 00 \tjmp 0xe4\t; 0xe4 <__ctors_end>");
}

void main() {
  auto tok2 = parseInstruction(123, "       0:\t0c 94 72 00 \tjmp\t0xe4, Z+\t; 0xe4 <__ctors_end>");
  auto tok3 = parseInstruction(123, "       0:\t0c 94 72 00 \tjmp\t0xe4\t; 0xe4 <__ctors_end>");
}
