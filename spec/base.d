module spec.base;
import machine.state;
import std.exception : enforce;
import std.conv;

alias cycleCount = uint;

abstract class Instruction(T) {
  cycleCount callback(T state);
  
    protected static uint parseNumericRegister(string param) {
        enforce(param[0] == 'r');
        return to!uint(param[1..$]);
    }

    string name; //e.g. ADDI 
    ulong address; //absolute address of instruction in memory
}

