module parser.parser;

import std.file;
import std.regex;
import std.conv;
import std.stdio;
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
}
