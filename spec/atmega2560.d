module spec.atmega2560; //we might want to make this a more generic AVR later on

import std.stdio : writeln;
import std.conv;

import spec.base;
import machine.state;
import std.bitmanip;
import core.bitop;

class Sreg : ReferenceRegister!ubyte {
    private bool getBit(uint bitNum) const {
        return to!bool(bytes()[0] & (1 << bitNum));
    }
    private bool setBit(uint bitNum, bool state) {
        if(state) {
            bytes[0] |= 1 << bitNum;
        } else {
            bytes[0] &= ~(1 << bitNum);
        }
        return state;
    }
    @property bool I() const { return getBit(7); } 
    @property bool I(bool newValue) { return setBit(7,newValue);}
    @property bool T() const { return getBit(6); } 
    @property bool T(bool newValue) { return setBit(6,newValue);}
    @property bool H() const { return getBit(5); } 
    @property bool H(bool newValue) { return setBit(5,newValue);}
    @property bool S() const { return getBit(4); } 
    @property bool S(bool newValue) { return setBit(4,newValue);}
    @property bool V() const { return getBit(3); } 
    @property bool V(bool newvalue) { return setBit(3,newvalue);}
    @property bool N() const { return getBit(2); } 
    @property bool N(bool newvalue) { return setBit(2,newvalue);}
    @property bool Z() const { return getBit(1); } 
    @property bool Z(bool newvalue) { return setBit(1,newvalue);}
    @property bool C() const { return getBit(0); } 
    @property bool C(bool newvalue) { return setBit(0,newvalue);}

    this(in ulong offset,Memory raw) {super("SREG",offset,raw);}
}

class InstructionsWrapper(T) {
    Instruction!T[] instructions;
    ulong currentInstruction = 0;
    uint averageSize;
    ulong addressOffset; //The instruction's offset in the real memory

    this(uint averageSize) {
        this.averageSize = averageSize;
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
            enforce(d == p || p == Direction.UNSET,"No instruction at specified address");
        }
        return instructions[guess];
    }
}

class AtMega2560State : MachineState {
    Memory data;
    Memory program;
    Memory eeprom;

    InstructionsWrapper!AtMega2560State instructions;

    ReferenceRegister!(ubyte)[32] valueRegisters;
    Sreg sreg;
    this() {
        data = new Memory(8 * 1024 + 512, 0);
        program = new Memory(256 * 1024, 0);
        eeprom = new Memory(4 * 1024, 0);
        sreg = new Sreg(0x5f,program);
        instructions = new InstructionsWrapper!AtMega2560State(2);
        for(int i = 0; i < valueRegisters.length; i++) {
            valueRegisters[i] = new ReferenceRegister!ubyte("r" ~ i.stringof, i, data); 
        }
    }

    @property Memory[string] memories() {
        return ["data": data, "program": program, "eeprom": eeprom];
    }

    @property Register[] registers() {
        return cast(Register[])valueRegisters;
    }

    cycleCount apply(Instruction!AtMega2560State instruction) {
        return instruction.callback(this);
    }
}

/** Add without Carry */
class Add : Instruction!AtMega2560State {
    uint dest;
    uint regToAdd;
    string name = "Add";

    this(in string[] parameters) {
        dest = parseNumericRegister(parameters[0]);
        regToAdd = parseNumericRegister(parameters[1]);
    }

    override cycleCount callback(AtMega2560State state) {
        state.valueRegisters[dest].bytes[0] +=
            state.valueRegisters[regToAdd].bytes[0];
        ubyte rd = state.valueRegisters[dest].bytes[0];
        ubyte rr = state.valueRegisters[regToAdd].bytes[0];
        bool rd7 = cast(bool) rd & 0b10000000;
        bool rr7 = cast(bool) rr & 0b10000000;
        state.sreg.C = rd7 & rr7 + rr7 & !rd7 + !rd7 &rd7;
        return 1;
    }
}

unittest {
    auto add = new Add(["r1", "r2"]);
    auto mem = new Memory(32, 0);
    auto register = new ReferenceRegister!ubyte("r2", 2, mem);
    //writeln(register.bytes);
    //register.bytes = [0x02];
    //writeln(register.bytes);

    //writeln(mem.data);
    //writeln(mem[1]);

    auto state = new AtMega2560State();
    state.valueRegisters[1].bytes[0] = 2;;
    state.valueRegisters[2].bytes[0] = 3;
    state.apply(add);
    writeln(state.data[0 .. 32]);
    assert(state.data[1] == 5);
}
