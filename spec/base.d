module spec.base;

import std.exception : enforce;
import std.conv;
import std.string;

import parser.parser;

alias cycleCount = uint;

class EOFException : Exception {
    this () {
        super("Last instruction reached");
    }
}

class InvalidAddressException : Exception {
    this(string msg) {
        super(msg);
    }
}

// XXX this interface is a hack until I figure out something better -Erik
interface InstructionI {
    public string toString() const;
}

abstract class Instruction(T) : InstructionI {
    cycleCount callback(T state) const;

    this(in InstructionToken token) {
        this.address = token.address;
        this.lineNumber = token.lineNumber;
        this.name = token.name;
        this.token = token;
    }

    public void optimize(in InstructionsWrapper!T instructions){}

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
        if (param.length > 2 && param[0..2] == "0x") {
            string numericPart = param[2..$];
            return parse!uint(numericPart, 16); // parse!int(param[2..$],16);
        }
        return to!uint(param);
    }

    public override string toString() const {
         return format("[%d] 0x%06x %s %s", lineNumber, address, name, token.parameters);
    }

    string name; //e.g. ADDI
    size_t address; //absolute address of instruction in memory
    size_t lineNumber;
    const InstructionToken token;
}

struct InstructionsWrapper(T) {
    private Instruction!T[] instructions;
    private size_t currentIndex = 0;
    private size_t nextIndex = 0;
    private size_t averageSize;
    private size_t addressOffset; //The instruction's offset in the real memory

    @property Instruction!T current() {
        if (currentIndex >= instructions.length) {
            throw new EOFException();
        }
        return instructions[currentIndex];
    }

    @property Instruction!T next() {
        debug if (nextIndex >= instructions.length) {
            return null;
        }
        return instructions[nextIndex];
    }

    this(Instruction!T[] instructions, size_t addressOffset = 0) {
        this.instructions = instructions;
        this.averageSize = averageInstructionSize(instructions);
        optimize();
    }

    protected void optimize() {
        foreach(instr; instructions) {
            try {
                instr.optimize(this);
            }
            //With PROGMEM we interpret data as instructions which want to jump
            //to invalid addresses. Ignore those.
            catch(InvalidAddressException e) {}
        }
    }

    private static size_t averageInstructionSize(in Instruction!T[] instructions)
    out(result) {
        if(instructions.length == 0) {
            assert(result == 0);
        } else {
            assert(result > 0);
        }
    }
    body {
        if(instructions.length <= 1) {
            return instructions.length;
        }
        size_t average = 0;
        size_t previousAddr = 0;
        foreach(i, instruction; instructions ){
            assert(instruction.address > previousAddr || i == 0);
            average += instruction.address - previousAddr;
            previousAddr = instruction.address;
        }
        return average / instructions.length;
    }

    invariant() {
        assert((currentIndex < instructions.length) || instructions.length == 0);
        assert((nextIndex <= instructions.length) || instructions.length == 0);
    }

    string toString() {
        return format("{[current: (%s, %d), next: (%s,%d)]", current.name,
                currentIndex, next, nextIndex);
    }

    Instruction!T fetch() {
        auto instr = next();
        debug if (instr is null) {
            throw new EOFException();
        }
        currentIndex = nextIndex;
        nextIndex = currentIndex + 1;

        return instr;
    }

    void relativeJump(size_t offset) {
        nextIndex = currentIndex + offset;
    }

    enum Direction {UP, DOWN, UNSET};
    /**
     If we want to find an instruction by address, we use our average
     instruction size heuristic to find the correct position in the array
     and then search until we find it.
      */
    size_t getInstructionIndex(size_t requestedAddress) const {
        assert(instructions.length > 0);

        size_t guess = (requestedAddress-addressOffset) / averageSize;
        if(guess >= instructions.length) {
            guess = instructions.length - 1;
        }
        Direction d = Direction.UNSET;
        Direction prev = Direction.UNSET;
        while(instructions[guess].address != requestedAddress) {
            prev = d;
            if(instructions[guess].address > requestedAddress) {
                if(guess <= 0)
                    throw new InvalidAddressException("Instruction not found(underflow in search)");
                guess--;
                d = Direction.DOWN;
            } else if(instructions[guess].address < requestedAddress) {
                if(guess >= instructions.length -1)
                    throw new InvalidAddressException("Instruction not found(overflow in search)");
                guess++;
                d = Direction.UP;
            }
            if(d != prev && prev != Direction.UNSET)
                throw new InvalidAddressException(
                        format("No instruction at specified address: %x", requestedAddress));
        }
        return guess;
    }

    void jump(size_t address) {
        nextIndex = getInstructionIndex(address);
    }

    void jumpIndex(size_t index) {
        nextIndex = index;
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

    wrapper.relativeJump(0);
    assert(wrapper.fetch().address == 0);
    wrapper.relativeJump(1);
    assert(wrapper.fetch().address == 2);
    wrapper.relativeJump(1);
    assert(wrapper.fetch().address == 4);
    wrapper.relativeJump(-2);
    assert(wrapper.fetch().address == 0);

    wrapper.jump(10);
    assert(wrapper.fetch().name == "fourth");
    bool exception = false;
    try {
        wrapper.jump(9);
    } catch (Exception e) {
        exception = true;
    }
    assert(exception);

    //Test with 1 instruction
    auto instr = new TestInstruction(0,"first");
    wrapper = new InstructionsWrapper!int([instr]);
    wrapper.jump(0);
    assert(wrapper.next() == instr);
}
