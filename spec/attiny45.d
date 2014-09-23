module spec.attiny45; //we might want to make this a more generic AVR later on

import std.stdio : writeln;

import spec.base;
import machine.state;

class AtTiny45State : MachineState {
  Memory romMemory;
  SimpleRegister!(ubyte)[32] valueRegisters;

  this() {
    auto test = new SimpleRegister!uint("henk", 0);
    for(int i = 0; i < valueRegisters.length; i++) {
      valueRegisters[i] = new SimpleRegister!ubyte("r" ~ i.stringof, 0); 
    }
  }

  @property Memory[string] memories() {
    return ["rom" : romMemory];
  }

  @property Register[] registers() {
    return cast(Register[])valueRegisters;
  }

}

class Adc : InstructionTemplate {

    uint dest;
    uint toAdd;

    string name = "adi";

    this(in string[] parameters) {
        assert(parameters[0][0] == 'r');
        string r = parameters[0][1 .. $];
        writeln(r);
    }

    override cycleCount callback(inout MachineState state) pure {
        AtTiny45State st = cast(AtTiny45State)state;
        st.valueRegisters[dest].value += st.valueRegisters[toAdd].value;
        return 1;
    }
}

void main()
{
    const auto adc = new Adc(["r1", "0x05"]);
}
