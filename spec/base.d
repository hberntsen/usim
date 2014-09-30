module spec.base;
import machine.state;
import std.exception : enforce;
import std.conv;
import parser.parser;

alias cycleCount = uint;

abstract class Instruction(T) {
    cycleCount callback(T state);
    this(in InstructionToken token) {
        this.address = token.address;
        this.name = token.name;
    }
    
    protected static uint parseNumericRegister(string param) {
        enforce(param[0] == 'r');
        return to!uint(param[1..$]);
    }

    string name; //e.g. ADDI 
    ulong address; //absolute address of instruction in memory
}

class InstructionsWrapper(T) {
    private Instruction!T[] instructions;
    private ulong currentInstruction = 0;
    private ulong averageSize;
    private ulong addressOffset; //The instruction's offset in the real memory

    this(Instruction!T[] instructions, ulong addressOffset = 0) {
        this.instructions = instructions;
        this.averageSize = averageInstructionSize(instructions);
    }

    private static ulong averageInstructionSize(in Instruction!T[] instructions)
    out(result) {
        assert((instructions.length == 0 && result == 0) || result > 0);
    }
    body {
        ulong average = 0;
        ulong previousAddr = 0;
        foreach(i,instruction; instructions){
            assert(instruction.address > previousAddr || i == 0);
            average += instruction.address - previousAddr;
            previousAddr = instruction.address;
        }
        return average / instructions.length;
    }

    invariant() {
        assert(currentInstruction < instructions.length 
                || instructions.length == 0);
    }

    Instruction!T relative(ulong offset) {
        currentInstruction += offset;
        return instructions[currentInstruction];
    }

    enum Direction {UP, DOWN, UNSET};
    /**
     If we want to find an instruction by address, we use our average
     instruction size heuristic to find the correct position in the array
     and then search until we find it.
      */
    Instruction!T absolute(ulong requestedAddress) {
        auto guess = (requestedAddress-addressOffset) / averageSize;
        if(guess >= instructions.length) { guess = instructions.length - 1;}
        Direction d = Direction.UNSET; 
        Direction p = Direction.UNSET;
        while(instructions[guess].address != requestedAddress) {
            p = d;
            if(instructions[guess].address > requestedAddress) {
                guess--;
                d = Direction.DOWN;
            } else if(instructions[guess].address < requestedAddress) {
                guess++;
                d = Direction.UP;
            }
            enforce(d == p || p == Direction.UNSET,
                    "No instruction at specified address");
        }
        currentInstruction = guess;
        return instructions[guess];
    }
}

unittest {
    class TestInstruction : Instruction!int {
        this (ulong address, string name) {
            this.name=name;
            this.address = address;
        }
        override cycleCount callback(int state) {
            return 1;
        }
    }

    auto wrapper = new InstructionsWrapper!int([
        new TestInstruction(0,"first"),
        new TestInstruction(2,"second"),
        new TestInstruction(4,"third-wide"),
        new TestInstruction(10,"fourth")
            ]);
    assert(wrapper.relative(0).address == 0);
    assert(wrapper.relative(1).address == 2);
    assert(wrapper.relative(1).address == 4);
    assert(wrapper.relative(-2).address == 0);
    assert(wrapper.absolute(10).name == "fourth");
    bool exception = false;
    try {
        wrapper.absolute(9); 
    } catch (Exception e) {
        exception = true;
    }
    assert(exception);
}
