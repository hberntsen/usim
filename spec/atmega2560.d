module spec.atmega2560; //we might want to make this a more generic AVR later on

import std.stdio : writeln;
import std.conv;

import spec.base;
import machine.state;
import std.bitmanip;
import core.bitop;
import parser.parser : InstructionToken;

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

class AtMega2560State : MachineState {
    Memory data;
    Memory program;
    Memory eeprom;
    Sreg sreg;
    ReferenceRegister!ushort stackPointer;
    protected InstructionsWrapper!AtMega2560State instructions;
    ReferenceRegister!(ubyte)[32] valueRegisters;
    SimpleRegister!ubyte result;

    invariant() {
        //As specified in the ATmega2560 manual
        assert(stackPointer.value > 0x0200);
    }

    this() {
        data = new Memory(8 * 1024 + 512, 0);
        program = new Memory(256 * 1024, 0);
        eeprom = new Memory(4 * 1024, 0);
        sreg = new Sreg(0x5f,program);
        //The stack pointer's initial value points to the end of the internal
        //SRAM: 8703
        stackPointer = new ReferenceRegister!ushort("SP",cast(ulong)0x5d, data);
        stackPointer.value = cast(ushort)(data.size - 2);

        instructions = new InstructionsWrapper!AtMega2560State([]);
        for(int i = 0; i < valueRegisters.length; i++) {
            valueRegisters[i] = new ReferenceRegister!ubyte("r" ~ i.stringof, i, data); 
        }
        result = new SimpleRegister!ubyte("R", 0);
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

    @property Instruction!AtMega2560State currentInstruction() {
        return instructions.current;
    }

    void jump(ulong address) {
        instructions.jump(address);
    }

    void relativeJump(int instructionOffset) {
        instructions.relativeJump(instructionOffset);
    }


}

/** Add without Carry */
class Add : Instruction!AtMega2560State {
    uint dest;
    uint regToAdd;

    this(in InstructionToken token) {
        super(token);
        dest = parseNumericRegister(token.parameters[0]);
        regToAdd = parseNumericRegister(token.parameters[1]);
    }

    override cycleCount callback(AtMega2560State state) const {
        ubyte rd = state.valueRegisters[dest].bytes[0];    //todo: dit met hele registers ipv 1 byte
        ubyte rr = state.valueRegisters[regToAdd].bytes[0];
        state.result.bytes[0] = rr + rd;
        state.valueRegisters[dest].bytes[0] = state.result.bytes[0];
        bool rd3 = cast(bool)(rd & 0b00000100); //todo: lelijk. 'bitslicen' en in de registerdefinitie stoppen
        bool rr3 = cast(bool)(rr & 0b00000100);
        bool r3 = cast(bool)(state.result.bytes[0] & 0b00000100);
        bool rd7 = cast(bool)(rd & 0b10000000);
        bool rr7 = cast(bool)(rr & 0b10000000);
        bool r7 = cast(bool)(state.result.bytes[0] & 0b10000000);
        state.sreg.H = rd3 && rr3 || rr3 && !r3 || !r3 && rd3;
        state.sreg.S = state.sreg.N ^ state.sreg.V;
        state.sreg.V = rd7 && rr7 && !r7 || !rd7 && !rr7 && r7;
        state.sreg.N = r7;
        state.sreg.Z = state.result.bytes[0] == 0;
        state.sreg.C = rd7 && rr7 || rr7 && !r7 || !r7 && rd7;
        return 1;
    }
}

class Nop : Instruction!AtMega2560State {
    this(in InstructionToken token) { super(token);}

    override cycleCount callback(AtMega2560State state) const {
        return 1;
    }
}

// Global interrupt disable
class Cli : Instruction!AtMega2560State {
    this(in InstructionToken token) { super(token); }

    override cycleCount callback(AtMega2560State state) const {
        state.sreg.I = false;
        return 1;
    }
}

// Compare with carry
class Cpc : Instruction!AtMega2560State {
    uint regd;
    uint regr;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
        regr = parseNumericRegister(token.parameters[1]);
    }

    override cycleCount callback(AtMega2560State state) const {
        auto rd = state.valueRegisters[regd].bytes[0];    //todo: dit met hele registers ipv 1 byte
        auto rr = state.valueRegisters[regr].bytes[0];
        bool rd3 = cast(bool)(rd & 0b00000100); //todo: lelijk. 'bitslicen' en in de registerdefinitie stoppen
        bool rr3 = cast(bool)(rr & 0b00000100);
        bool r3 = cast(bool)(state.result.bytes[0] & 0b00000100);
        bool rd7 = cast(bool)(rd & 0b10000000);
        bool rr7 = cast(bool)(rr & 0b10000000);
        bool r7 = cast(bool)(state.result.bytes[0] & 0b10000000);
        state.sreg.H = !rd3 && rr3 || rr3 && r3 || r3 && !rd3;
        state.sreg.S = state.sreg.N ^ state.sreg.V;
        state.sreg.V = rd7 && !rr7 && !r7 || !rd7 && rr7 && r7;
        state.sreg.N = r7;
        if(state.result != 0)
            state.sreg.Z = false;
        state.sreg.C = !rd7 && rr7 || rr7 && r7 || r7 && !rd7;
        return 1;
    }
}

/* Compare with immediate */
class Cpi : Instruction!AtMega2560State {
    uint regd;
    uint k;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
        k = parseInt(token.parameters[1]);
    }

    override cycleCount callback(AtMega2560State state) const {
        auto rd = state.valueRegisters[regd].bytes[0];    //todo: dit met hele registers ipv 1 byte
        bool rd3 = cast(bool)(rd & 0b00000100); //todo: lelijk. 'bitslicen' en in de registerdefinitie stoppen
        bool k3 = cast(bool)(k & 0b00000100);
        bool r3 = cast(bool)(state.result.bytes[0] & 0b00000100);
        bool rd7 = cast(bool)(rd & 0b10000000);
        bool k7 = cast(bool)(k & 0b10000000);
        bool r7 = cast(bool)(state.result.bytes[0] & 0b10000000);
        state.sreg.H = !rd3 && k3 || k3 && r3 || r3 && !rd3;
        state.sreg.S = state.sreg.N ^ state.sreg.V;
        state.sreg.V = rd7 && !k7 && !r7 || !rd7 && k7 && r7;
        state.sreg.N = r7;
        state.sreg.Z = state.result.bytes[0] == 0;
        state.sreg.C = !rd7 && k7 || k7 && r7 || r7 && !rd7;
        return 1;
    }
}

/* Exclusive OR */
class Eor : Instruction!AtMega2560State {
    uint regd;
    uint regr;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
        regr = parseNumericRegister(token.parameters[1]);
    }

    override cycleCount callback(AtMega2560State state) const {
        ubyte rd = state.valueRegisters[regd].bytes[0];    //todo: dit met hele registers ipv 1 byte
        ubyte rr = state.valueRegisters[regr].bytes[0];
        state.result.bytes[0] = rr ^ rd;
        state.valueRegisters[regd].bytes[0] = state.result.bytes[0];
        bool r7 = cast(bool)(state.result.bytes[0] & 0b10000000);
        state.sreg.S = state.sreg.N ^ state.sreg.V;
        state.sreg.V = false;
        state.sreg.N = r7;
        state.sreg.Z = state.result.bytes[0] == 0;
        return 1;
    }
}


//TODO: fix this test for our add instruction
/*
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
} */
