module spec.attiny45; //we might want to make this a more generic AVR later on

import spec.base;
import machine.state;

class AtTiny45State : MachineState {
  Memory romMemory;
  SimpleRegister!ubyte[32] valueRegisters;

  this() {
    test = new SimpleRegister!uint("henk", 0);
    for(int i = 0; i < valueRegisters.length; i++) {
      valueRegisters[i] = new SimpleRegister!ubyte("r" ~ i.stringof, 0); 
    }
  }

  @property Memory[string] memories() {
    return ["rom" : romMemory];
  }

  @property Register[] registers() {
    return [];
  }

}

class Adc : InstructionTemplate {

  uint dest;
  uint toAdd;

  this(string[] parameters) {
    name = "addi"; 
  }

  override cycleCount callback(inout MachineState state) pure {
    AtTiny45State st = cast(AtTiny45State)state;
    st.valueRegisters[dest].value += st.valueRegisters[toAdd].value;
    return 0;
  }
}



