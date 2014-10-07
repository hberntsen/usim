module spec.base;
import machine.state;
import std.exception : enforce;
import std.conv;
import parser.parser;

alias cycleCount = uint;

abstract class Instruction(T) {
    cycleCount callback(T state) const;
    this(in InstructionToken token) {
        this.address = token.address;
        this.name = token.name;
    }

    protected static uint parseNumericRegister(string param) {
        enforce(param[0] == 'r');
        return to!uint(param[1..$]);
    }

    protected static int parseInt(string param) {
        if(param[0] == '+' || param[0] == '.') {
            return to!int(param[1..$]);
        }
        return to!int(param);
    }

    protected static uint parseHex(string param) {
        assert(param[0..2] == "0x");
        string numericPart = param[2..$];
        return parse!uint(numericPart, 16); // parse!int(param[2..$],16);
    }

    string name; //e.g. ADDI
    size_t address; //absolute address of instruction in memory
}

class InstructionsWrapper(T) {
    private Instruction!T[] instructions;
    private size_t currentIndex = 0;
    private size_t averageSize;
    private size_t addressOffset; //The instruction's offset in the real memory

    @property Instruction!T current() { return instructions[currentIndex];}

    this(Instruction!T[] instructions, size_t addressOffset = 0) {
        this.instructions = instructions;
        this.averageSize = averageInstructionSize(instructions);
    }

    private static size_t averageInstructionSize(in Instruction!T[] instructions)
    out(result) {
        assert((instructions.length == 0 && result == 0) || result > 0);
    }
    body {
        size_t average = 0;
        size_t previousAddr = 0;
        foreach(i,instruction; instructions){
            assert(instruction.address > previousAddr || i == 0);
            average += instruction.address - previousAddr;
            previousAddr = instruction.address;
        }
        return average / instructions.length;
    }

    invariant() {
        assert(currentIndex < instructions.length
                || instructions.length == 0);
    }

    Instruction!T relativeJump(size_t offset) {
        currentIndex += offset;
        return instructions[currentIndex];
    }

    enum Direction {UP, DOWN, UNSET};
    /**
     If we want to find an instruction by address, we use our average
     instruction size heuristic to find the correct position in the array
     and then search until we find it.
      */
    Instruction!T jump(size_t requestedAddress) {
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
        currentIndex = guess;
        return instructions[guess];
    }
}

unittest {
    class TestInstruction : Instruction!int {
        this (size_t address, string name) {
            InstructionToken token = new InstructionToken(0, address,
                    [],name,[]);
            super(token);
        }
        override cycleCount callback(int state) const {
            return 1;
        }
    }

    auto wrapper = new InstructionsWrapper!int([
        new TestInstruction(0,"first"),
        new TestInstruction(2,"second"),
        new TestInstruction(4,"third-wide"),
        new TestInstruction(10,"fourth")
            ]);

    assert(wrapper.relativeJump(0).address == 0);
    assert(wrapper.relativeJump(1).address == 2);
    assert(wrapper.relativeJump(1).address == 4);
    assert(wrapper.relativeJump(-2).address == 0);
    assert(wrapper.jump(10).name == "fourth");
    bool exception = false;
    try {
        wrapper.jump(9);
    } catch (Exception e) {
        exception = true;
    }
    assert(exception);
}
