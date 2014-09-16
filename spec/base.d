module spec.base;
import machine.state;

alias cycleCount = uint;

abstract class InstructionTemplate {
  cycleCount callback(inout MachineState state) pure;
  string name; //e.g. ADDI 
}

