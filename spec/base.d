module spec.base;
import machine.state;

alias cycleCount = uint;

abstract class Instruction {
  cycleCount callback(inout MachineState state);

  string name; //e.g. ADDI 
}

