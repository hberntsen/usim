module spec.atmega2560; //we might want to make this a more generic AVR later on

import std.stdio : writeln;
import std.conv;
import std.system;

import spec.base;
import machine.state;
import std.bitmanip;
import core.bitop;
import parser.parser : InstructionToken;

class Sreg : ReferenceRegister!ubyte {
    private bool getBit(uint bitNum) const {
        return cast(bool)(bytes()[0] & (1 << bitNum));
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

    this(in size_t offset,Memory raw) {super("SREG",offset,raw);}
}
unittest {
    Memory mem = new Memory(1,0);
    Sreg sreg = new Sreg(0,mem);
    assert(mem[0] == 0);
    sreg.Z = true;
    assert(sreg.Z = true);
    assert(mem[0] == 0b00000010);
}

class AtMega2560State : MachineState {
    Memory data;
    Memory program;
    Memory eeprom;
    Sreg sreg;
    ReferenceRegister!ushort stackPointer;
    protected InstructionsWrapper!AtMega2560State instructions;
    ReferenceRegister!(ubyte)[32] valueRegisters;

    invariant() {
        //As specified in the ATmega2560 manual
        assert(stackPointer.value > 0x0200);
    }

    @property size_t programCounter() { return instructions.current.address/2; }
    @property size_t programCounter(size_t newpc) {
        instructions.jump(newpc*2);
        return programCounter();
    }

    this() {
        data = new Memory(8 * 1024 + 512, 0);
        program = new Memory(256 * 1024, 0);
        eeprom = new Memory(4 * 1024, 0);
        sreg = new Sreg(0x5f,program);
        //The stack pointer's initial value points to the end of the internal
        //SRAM: 8703
        stackPointer = new ReferenceRegister!ushort("SP",cast(size_t)0x5d, data);
        stackPointer.value = cast(ushort)(data.size - 2);

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

    @property Instruction!AtMega2560State currentInstruction() {
        return instructions.current;
    }

    void setInstructions(Instruction!AtMega2560State[] instructions) {
        this.instructions = new InstructionsWrapper!AtMega2560State(instructions);
    }

    void jump(size_t address) {
        instructions.jump(address);
    }

    void relativeJump(int instructionOffset) {
        instructions.relativeJump(instructionOffset);
    }

    void setSregLogical(ubyte result) {
      sreg.V = false;
      sreg.N = cast(bool)(result & 0b1000000);
      sreg.S = sreg.V ^ sreg.N;
      sreg.Z = result == 0;
    }

    void setSregArithPos(ubyte a, ubyte b, ubyte c) {
      bool[] bits = getRelevantBits(a, b, c);
      sreg.H = bits[0] && bits[2] || bits[2] && !bits[4] || !bits[4] && bits[0];
      sreg.V = bits[1] && bits[3] && !bits[5] || !bits[1] && !bits[3] && bits[5];
      sreg.N = bits[5];
      sreg.S = sreg.V ^ sreg.N;
      sreg.Z = c == 0;
      sreg.C = bits[1] && bits[3] || bits[3] && !bits[5] || !bits[5] && bits[1];
    }

    void setSregArithNeg(ubyte a, ubyte b, ubyte c, bool preserveZ = false) {
      bool[] bits = getRelevantBits(a, b, c);
      sreg.H = !bits[0] && bits[2] || bits[2] && bits[4] || bits[4] && !bits[0];
      sreg.V = bits[1] && !bits[3] && !bits[5] || !bits[1] && bits[3] && bits[5];
      sreg.N = bits[5];
      sreg.S = sreg.V ^ sreg.N;
      if(preserveZ && c != 0)
        sreg.Z = false;
      else if(!preserveZ)
        sreg.Z = c == 0;
      sreg.C = !bits[1] && bits[3] || bits[3] && bits[5] || bits[5] && !bits[1];
    }

    private bool[] getRelevantBits(ubyte a, ubyte b, ubyte c) {
      return [cast(bool)(a & 0b00001000), cast(bool)(a & 0b1000000), cast(bool)(b & 0b00001000), cast(bool)(b & 0b1000000), cast(bool)(c & 0b00001000), cast(bool)(c & 0b1000000)];
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
        ubyte rd = state.valueRegisters[dest].value;
        ubyte rr = state.valueRegisters[regToAdd].value;
        ubyte result = cast(ubyte)(rr + rd);
        state.valueRegisters[dest].value = result;
        state.setSregArithPos(rd, rr, result);
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
        auto rd = state.valueRegisters[regd].value;
        auto rr = state.valueRegisters[regr].value;
        ubyte result = cast(ubyte)(rd - rr - state.sreg.C); //todo: unittesten
        state.setSregArithNeg(rd, rr, result, true);
        return 1;
    }
}

/* Compare with immediate */
class Cpi : Instruction!AtMega2560State {
    uint regd;
    ubyte k;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
        k = cast(ubyte)parseHex(token.parameters[1]);
    }

    override cycleCount callback(AtMega2560State state) const {
        auto rd = state.valueRegisters[regd].value;
        ubyte result = cast(ubyte)(rd - k); //todo: unittesten
        state.setSregArithNeg(rd, k, result);
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
        ubyte rd = state.valueRegisters[regd].value;
        ubyte rr = state.valueRegisters[regr].value;
        ubyte result = rr ^ rd;
        state.valueRegisters[regd].value = result;
        state.setSregLogical(result);
        return 1;
    }
}

/* Load immediate */
class Ldi : Instruction!AtMega2560State {
    uint regd;
    uint k;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
        k = parseHex(token.parameters[1]);
    }

    override cycleCount callback(AtMega2560State state) const {
        state.valueRegisters[regd].value = cast(ubyte)k;
        return 1;
    }
}

class Rjmp : Instruction!AtMega2560State {
    const int jumpOffset;

    this(in InstructionToken token) {
        super(token);
        jumpOffset = parseInt(token.parameters[0]);
        assert(jumpOffset <= 2000);
        assert(-2000 <= jumpOffset);
    }

    override cycleCount callback(AtMega2560State state) const {
        state.relativeJump(jumpOffset);
        return 2;
    }
}
unittest {
    auto state = new AtMega2560State();
    auto rjmp = new Rjmp(new InstructionToken(0,0,[],"rjmp",[".+2"]));
    auto nop1 = new Nop(new InstructionToken(0,2,[],"nop1",[]));
    auto rjmp2 = new Rjmp(new InstructionToken(0,4,[],"rjmp",["-1"]));
    state.setInstructions([rjmp,nop1,rjmp2]);
    auto cycles = rjmp.callback(state);
    assert(cycles == 2);
    assert(state.currentInstruction.address == 4);
    rjmp2.callback(state);
    assert(state.currentInstruction.address == 2);
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
