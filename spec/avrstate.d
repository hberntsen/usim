module spec.avrstate;

import std.stdio;
import std.conv;
import std.system;
import std.string;

import spec.base;
import machine.state;
import std.bitmanip;
import core.bitop;
import parser.parser : InstructionToken;

final class Sreg : ReferenceRegister!ubyte {
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

    override string toString() {
        return format("%s", [
            "I": I(),
            "T": T(),
            "H": H(),
            "S": S(),
            "V": V(),
            "N": N(),
            "Z": Z(),
            "C": C(),
        ]);
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

    public void setAll(bool C, bool Z, bool N, bool V, bool S, bool H, bool T, bool I) {
        this.value = C | (Z << 1) | (N << 2) | (V << 3) | (S << 4) | (H << 5) | (T << 6) | (I << 7);
    }

    this(in size_t offset,Memory raw) {super("SREG",offset,raw);}
}
unittest {
    Memory mem = new Memory(1,0);
    Sreg sreg = new Sreg(0,mem);
    assert(mem[0] == 0);
    sreg.Z = true;
    assert(sreg.Z = true);
    assert(mem[0] == 0b00000010);

    sreg.setAll(false, true, false,true,false,false,false,false);
    assert(mem[0] == 0b00001010);
}

class AvrState : MachineState {
    Memory data;
    Memory program;
    Memory eeprom;
    Sreg sreg;
    ReferenceRegister!ushort xreg, yreg, zreg;
    ReferenceRegister!ushort stackPointer;
    private InstructionsWrapper!AvrState instructions;
    ReferenceRegister!(ubyte)[32] valueRegisters;
    ReferenceRegister!(ubyte)[64] ioRegisters;
    private ReferenceRegister!ushort[string] _refregs;

    invariant() {
        //As specified in the ATmega2560 manual
        assert(stackPointer.value > 0x0200);
    }

    @property final size_t programCounter() {
        return instructions.next.address/2;
    }
    @property final size_t programCounter(size_t newpc) {
        instructions.jump(newpc*2);
        return programCounter();
    }

    this(size_t dataSize = 8*1024+512,
            size_t programSize = 256 * 1024,
            size_t eepromSize = 4 * 1024,
            size_t sregOffset = 0x5f,
            size_t spOffset = 0x5d) {
        data = new Memory(dataSize, 0);
        program = new Memory(programSize, 0);
        eeprom = new Memory(eepromSize, 0);

        sreg = new Sreg(sregOffset,program);
        xreg = new ReferenceRegister!ushort("X", 26, data);
        yreg = new ReferenceRegister!ushort("Y", 28, data);
        zreg = new ReferenceRegister!ushort("Z", 30, data);
        _refregs = ["X": xreg, "Y": yreg, "Z": zreg];
        //The stack pointer's initial value points to the end of the internal
        //SRAM: 8703
        stackPointer = new ReferenceRegister!ushort("SP",spOffset, data);
        stackPointer.value = cast(ushort)(data.size - 2);

        for(int i = 0; i < valueRegisters.length; i++) {
            valueRegisters[i] = new ReferenceRegister!ubyte("r" ~ i.stringof, i, data);
        }

        for(int i = 0; i < ioRegisters.length; i++) {
            ioRegisters[i] = new ReferenceRegister!ubyte(format("io:%x",
                        cast(size_t)(i + 0x20)), i + 0x40, data);
        }
    }
    final {

        @property ReferenceRegister!ushort[string] refregs() {
            return _refregs;
        }

        @property Memory[string] memories() {
            return ["data": data, "program": program, "eeprom": eeprom];
        }

        @property Register[] registers() {
            return cast(Register[])valueRegisters;
        }

        @property Instruction!AvrState currentInstruction() {
            return instructions.current;
        }

        @property Instruction!AvrState nextInstruction() {
            return instructions.next;
        }

        Instruction!AvrState fetchInstruction() {
            return instructions.fetch();
        }

        void setInstructions(Instruction!AvrState[] instructions) {
            this.instructions = new InstructionsWrapper!AvrState(instructions);
        }

        void jump(size_t address) {
            instructions.jump(address);
        }

        void jumpIndex(size_t index) {
            instructions.jumpIndex(index);
        }

        void relativeJump(in int instructionOffset) {
            instructions.relativeJump(instructionOffset);
        }

        void setSregLogical(ubyte result) {
            sreg.V = false;
            sreg.N = cast(bool)(result & 0b10000000);
            sreg.S = sreg.V ^ sreg.N;
            sreg.Z = result == 0;
        }

        void setSregArithPos(ubyte a, ubyte b, ubyte c) {
            immutable bool[6] bits = getRelevantBits(a, b, c);
            bool H = bits[0] && bits[2] || bits[2] && !bits[4] || !bits[4] && bits[0];
            bool V = bits[1] && bits[3] && !bits[5] || !bits[1] && !bits[3] && bits[5];
            bool N = bits[5];
            // todo: compute S before V & N or after?
            bool S = sreg.V ^ sreg.N;
            bool Z = c == 0;
            bool C = bits[1] && bits[3] || bits[3] && !bits[5] || !bits[5] && bits[1];
            sreg.setAll(C,Z,N,V,S,H,sreg.T,sreg.I);
        }

        void setSregArithNeg(ubyte a, ubyte b, ubyte c, bool preserveZ = false) {
            immutable bool[6] bits = getRelevantBits(a, b, c);
            bool H = !bits[0] && bits[2] || bits[2] && bits[4] || bits[4] && !bits[0];
            bool V = bits[1] && !bits[3] && !bits[5] || !bits[1] && bits[3] && bits[5];
            bool N = bits[5];
            bool S = V ^ N;
            bool Z = sreg.Z;
            if(preserveZ && c != 0)
                Z = false;
            else if(!preserveZ)
                Z = c == 0;
            bool C = !bits[1] && bits[3] || bits[3] && bits[5] || bits[5] && !bits[1];

            sreg.setAll(C,Z,N,V,S,H,sreg.T,sreg.I);
        }

        ReferenceRegister!ubyte setIoRegisterByIo(size_t addr, ubyte value) {
            this.ioRegisters[addr - 0x20].value = value;
            return this.ioRegisters[addr - 0x20];
        }

        ReferenceRegister!ubyte setIoRegisterByData(size_t addr, ubyte value) {
            this.ioRegisters[addr - 0x40].value = value;
            return this.ioRegisters[addr - 0x40];
        }

        ReferenceRegister!ubyte getIoRegisterByIo(size_t addr) {
            return this.ioRegisters[addr - 0x20];
        }

        ReferenceRegister!ubyte getIoRegisterByData(size_t addr) {
            return this.ioRegisters[addr - 0x40];
        }
    }

    private static bool[6] getRelevantBits(ubyte a, ubyte b, ubyte c) {
        return [cast(bool)(a & 0b00001000), cast(bool)(a & 0b10000000),
               cast(bool)(b & 0b00001000), cast(bool)(b & 0b10000000),
               cast(bool)(c & 0b00001000), cast(bool)(c & 0b10000000)];
    }
}

/** Add without Carry */
class Add : Instruction!AvrState {
    uint dest;
    uint regToAdd;

    this(in InstructionToken token) {
        super(token);
        dest = parseNumericRegister(token.parameters[0]);
        regToAdd = parseNumericRegister(token.parameters[1]);
    }

    override cycleCount callback(AvrState state) const {
        ubyte rd = state.valueRegisters[dest].value;
        ubyte rr = state.valueRegisters[regToAdd].value;
        ubyte result = cast(ubyte)(rr + rd);
        state.valueRegisters[dest].value = result;
        state.setSregArithPos(rd, rr, result);

        return 1;
    }
}

class Adc : Instruction!AvrState {
    uint dest;
    uint regToAdd;

    this(in InstructionToken token) {
        super(token);
        dest = parseNumericRegister(token.parameters[0]);
        regToAdd = parseNumericRegister(token.parameters[1]);
    }

    override cycleCount callback(AvrState state) const {
        ubyte rd = state.valueRegisters[dest].value;
        ubyte rr = state.valueRegisters[regToAdd].value;
        ubyte result = cast(ubyte)(rr + rd + state.sreg.C);
        state.valueRegisters[dest].value = result;
        state.setSregArithPos(rd, rr, result);

        return 1;
    }
}

unittest {
    auto state = new AvrState();
    auto adc = new Adc(new InstructionToken(0,0,[],"adc",["r2", "r0"]));
    auto adc2 = new Adc(new InstructionToken(0,0,[],"adc",["r3", "r1"]));

    state.sreg.C = false;
    state.setInstructions([adc]);

    // low bytes
    state.valueRegisters[0] = 0x81;
    state.valueRegisters[2] = 0x82;

    // high bytes
    state.valueRegisters[1] = 0x00;
    state.valueRegisters[3] = 0x10;

    adc.callback(state);
    assert(state.valueRegisters[2].value == 0x03);
    assert(state.sreg.C == true);

    adc2.callback(state);
    assert(state.sreg.C == false);
    assert(state.valueRegisters[3].value = 0x11);
}

class Adiw : Instruction!AvrState {
    const uint dest;
    const uint k;

    this(in InstructionToken token) {
        super(token);
        dest = parseNumericRegister(token.parameters[0]);
        k = parseHex(token.parameters[1]);
    }

    override cycleCount callback(AvrState state) const {
        uint rdlow = state.valueRegisters[dest].value;
        uint rdhigh = cast(uint)(state.valueRegisters[dest + 1].value) << 7;

        uint rd = rdlow + rdhigh;
        uint result = rd + k;

        state.valueRegisters[dest].value = cast(ubyte)(result & 0x00ff);
        state.valueRegisters[dest + 1].value = cast(ubyte)((result >> 8) & 0x00ff);

        bool rdh0 = cast(bool)(rdhigh & 0x01);
        bool rdh7 = cast(bool)(rdhigh & 0x80);
        bool rdl0 = cast(bool)(rdlow & 0x01);
        bool rdl7 = cast(bool)(rdlow & 0x80);
        bool r0 = cast(bool)(result & 0x0001);
        bool r7 = cast(bool)(result & 0x0080);
        bool r15 = cast(bool)(result & 0x8000);

        state.sreg.N = r15;
        state.sreg.V = !rdh7 && r15;
        state.sreg.S = state.sreg.N ^ state.sreg.V;
        state.sreg.C = r15 && rdh7;
        state.sreg.Z = result == 0;

        return 2;
    }
}

unittest {
    auto state = new AvrState();
    auto adiw = new Adiw(new InstructionToken(0,0,[],"adiw",["r30", "0x01"]));

    state.setInstructions([adiw]);

    state.valueRegisters[30] = 0xff;
    state.valueRegisters[31] = 0x00;

    adiw.callback(state);
    assert(state.valueRegisters[30].value == 0x00);
    assert(state.valueRegisters[31].value == 0x01);
    assert(state.sreg.C == false);
}

class And : Instruction!AvrState {
    uint regd;
    uint regr;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
        regr = parseNumericRegister(token.parameters[1]);
    }

    override cycleCount callback(AvrState state) const {
        ubyte rd = state.valueRegisters[regd].value;
        ubyte rr = state.valueRegisters[regr].value;
        ubyte result = rr & rd;
        state.valueRegisters[regd].value = result;
        state.setSregLogical(result);
        return 1;
    }
}

class Andi : Instruction!AvrState {
    uint regd;
    uint k;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
        k = parseHex(token.parameters[1]);
    }

    override cycleCount callback(AvrState state) const {
        ubyte rd = state.valueRegisters[regd].value;
        ubyte result = rd & cast(ubyte)(k);
        state.valueRegisters[regd].value = result;
        state.setSregLogical(result);
        return 1;
    }
}

abstract class RelativeBranchInstruction : Instruction!AvrState {
    size_t dest;

    this(in InstructionToken token) {
        super(token);
        auto relativePc = parseInt(token.parameters[0]);
        assert(relativePc <= 63 * 2 && relativePc >= -64 * 2);
        dest = address + 2 + relativePc;
    }

    override void optimize(in InstructionsWrapper!AvrState iw) {
        dest = iw.getInstructionIndex(dest);
    }

    override cycleCount callback(AvrState state) const {
        if(check(state)) {
            state.jumpIndex(dest);
            return 2;
        } else {
            return 1;
        }
    }

    bool check(AvrState state) const;
}

class Brcc : RelativeBranchInstruction {
    this(in InstructionToken token) {
        super(token);
    }

    override bool check(AvrState state) const {
        return state.sreg.C;
    }
}

class Brcs : RelativeBranchInstruction {
    this(in InstructionToken token) {
        super(token);
    }

    override bool check(AvrState state) const {
        return !state.sreg.C;
    }
}

class Breq : RelativeBranchInstruction {
    this(in InstructionToken token) {
        super(token);
    }

    override bool check(AvrState state) const {
        return state.sreg.Z;
    }
}

class Brge : RelativeBranchInstruction {
    this(in InstructionToken token) {
        super(token);
    }

    override bool check(AvrState state) const {
        return !state.sreg.S;
    }
}

class Brlt : RelativeBranchInstruction {
    this(in InstructionToken token) {
        super(token);
    }

    override bool check(AvrState state) const {
        return state.sreg.S;
    }
}


class Brne : RelativeBranchInstruction {
    this(in InstructionToken token) {
        super(token);
    }
    override bool check(AvrState state) const {
        return !state.sreg.Z;
    }
}

class Brtc : RelativeBranchInstruction {
    this(in InstructionToken token) {
        super(token);
    }
    override bool check(AvrState state) const {
        return !state.sreg.T;
    }
}

unittest {
    auto state = new AvrState();
    state.sreg.Z = false;
    //Jump to nop2 if Z is cleared (false)
    auto brne = new Brne(new InstructionToken(0,0,[],"brne",[".+2"]));
    auto nop = new Nop(new InstructionToken(0,2,[],"nop",[]));
    auto nop2 = new Nop(new InstructionToken(0,4,[],"nop",[]));
    state.setInstructions([brne,nop,nop2]);
    brne.callback(state);
    assert(state.fetchInstruction().address == 4);

    state.relativeJump(-2);
    state.sreg.Z = true;
    auto instr = state.fetchInstruction();
    assert(instr == brne);
    brne.callback(state);
    assert(state.fetchInstruction().address == 2);
}

/** Long Call to a Subroutine */
class Call : Instruction!AvrState {
    size_t dest;

    this(in InstructionToken token) {
        super(token);
        dest = parseHex(token.parameters[0]);
    }

    override void optimize(in InstructionsWrapper!AvrState iw) {
        dest = iw.getInstructionIndex(dest);
    }

    override cycleCount callback(AvrState state) const {
        //Program counter points to next instruction, we want that address on
        //the stack
        size_t pc = state.programCounter;
        state.data[state.stackPointer.value -2 .. state.stackPointer.value+1] =
            [cast(ubyte)(pc), cast(ubyte)(pc >>> 8), cast(ubyte)(pc >>> 16)];
        state.stackPointer.value = cast(ushort)(state.stackPointer.value - 3);

        state.jumpIndex(dest);

        return 5;
    }
}

///Tests both call and ret
unittest {
    auto state = new AvrState();
    auto nop0 = new Nop(new InstructionToken(0,0,[],"nop",[]));
    //call jumps to the ret instruction
    auto call = new Call(new InstructionToken(0,2,[],"call",["0x8"]));
    auto nop1 = new Nop(new InstructionToken(0,6,[],"nop",[]));
    auto ret = new Ret(new InstructionToken(0,8,[],"ret",[]));
    state.setInstructions([nop0,call,nop1,ret]);

    ushort spInit = state.stackPointer.value;

    state.fetchInstruction().callback(state); //nop
    auto instr = state.fetchInstruction();
    assert(instr == call);
    call.callback(state);

    assert(state.stackPointer.value == spInit - 3);
    //Program counter that is stored on the stack should be 3, the program counter is 1 before call is executed,
    // then raised to a value of 3 since the call instruction takes 4 bytes
    assert(state.programCounter == 4);
    assert(state.data[spInit-2] == 3);

    //Done testing call, now test ret
    instr = state.fetchInstruction();
    assert(instr == ret);
    ret.callback(state);
    assert(state.programCounter == 3);
    assert(state.stackPointer.value == spInit);
}

// Global interrupt disable
class Cli : Instruction!AvrState {
    this(in InstructionToken token) { super(token); }

    override cycleCount callback(AvrState state) const {
        state.sreg.I = false;
        return 1;
    }
}

class Clt : Instruction!AvrState {
    this(in InstructionToken token) {
        super(token);
    }

    override cycleCount callback(AvrState state) const {
        state.sreg.T = false;
        return 1;
    }
}

class Com : Instruction!AvrState {
    uint regd;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
    }

    override cycleCount callback(AvrState state) const {
        ubyte rd = state.valueRegisters[regd].value;
        ubyte result = ~rd;
        state.valueRegisters[regd].value = result;
        state.setSregLogical(result);
        state.sreg.C = true;
        return 1;
    }
}

unittest {
    auto state = new AvrState();
    auto com = new Com(new InstructionToken(0, 0, [], "com", ["r0"]));
    state.setInstructions([com]);
    state.valueRegisters[0].value = 0xf0;

    com.callback(state);
    assert(state.valueRegisters[0].value == 0x0f);
    assert(state.sreg.C);
}

class Cp : Instruction!AvrState {
    uint regd;
    uint regr;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
        regr = parseNumericRegister(token.parameters[1]);
    }

    override cycleCount callback(AvrState state) const {
        auto rd = state.valueRegisters[regd].value;
        auto rr = state.valueRegisters[regr].value;
        ubyte result = cast(ubyte)(rd - rr);
        state.setSregArithNeg(rd, rr, result);
        return 1;
    }
}

// Compare with carry
class Cpc : Instruction!AvrState {
    uint regd;
    uint regr;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
        regr = parseNumericRegister(token.parameters[1]);
    }

    override cycleCount callback(AvrState state) const {
        auto rd = state.valueRegisters[regd].value;
        auto rr = state.valueRegisters[regr].value;
        ubyte result = cast(ubyte)(rd - rr - state.sreg.C); //todo: unittesten
        state.setSregArithNeg(rd, rr, result, true);
        return 1;
    }
}

/* Compare with immediate */
class Cpi : Instruction!AvrState {
    uint regd;
    ubyte k;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
        k = cast(ubyte)parseHex(token.parameters[1]);
    }

    override cycleCount callback(AvrState state) const {
        auto rd = state.valueRegisters[regd].value;
        ubyte result = cast(ubyte)(rd - k); //todo: unittesten
        state.setSregArithNeg(rd, k, result);
        return 1;
    }
}

/** Compare Skip if Equal */
class Cpse : SkipInstruction {
    uint rd;
    uint rr;

    this(in InstructionToken token) {
        super(token);
        rd = parseNumericRegister(token.parameters[0]);
        rr = parseNumericRegister(token.parameters[1]);
    }

    override bool shouldSkip(AvrState state) const {
        return state.valueRegisters[rd].value ==
            state.valueRegisters[rr].value;
    }
}

class Dec : Instruction!AvrState {
    uint reg;

    this(in InstructionToken token) {
        super(token);
        reg = parseNumericRegister(token.parameters[0]);
    }

    override cycleCount callback(AvrState state) const {
        ubyte rd = state.valueRegisters[reg].value;
        ubyte result = cast(ubyte)(rd - 1);
        state.valueRegisters[reg].value = result;

        state.sreg.V = rd == 0x80;
        state.sreg.N = cast(bool)(result & 0x80);
        state.sreg.S = state.sreg.N ^ state.sreg.V;
        state.sreg.Z = result == 0;

        return 1;
    }
}

class Eicall : Instruction!AvrState {
    this(in InstructionToken token) {
        super(token);
    }

    override cycleCount callback(AvrState state) const {
        //todo: remove new and do it like hans in rcall
        ubyte[] pcBytes = new ubyte[size_t.sizeof];

        //Convert PC+1 to bytes and store it on the stack
        pcBytes.write!(size_t, Endian.littleEndian)(state.programCounter, 0);
        state.data[state.stackPointer.value -2 .. state.stackPointer.value+1] =
            pcBytes[0 .. 3];

        state.stackPointer.value = cast(ushort)(state.stackPointer.value - 3);

        size_t z = cast(size_t)(state.zreg);
        size_t eind = state.getIoRegisterByIo(0x3c);

        size_t address = (z & 0x00ffff) + ((eind & 0x00ffff) << 16);
        writefln("%x, %x -> %x, %x -> %x", z, eind, z & 0x00ffff, (eind & 0x00ffff) << 16, address);

        state.jump(address);

        return 4; // NOTE: 3 on XMEGA
    }
}

unittest {
    auto state = new AvrState();
    auto nop0 = new Nop(new InstructionToken(0,0,[],"nop",[]));
    //rcall jumps to the ret instruction
    auto eicall = new Eicall(new InstructionToken(0,2,[],"eicall",[]));
    auto nop1 = new Nop(new InstructionToken(0,4,[],"nop",[]));
    auto ret = new Ret(new InstructionToken(0,0x211001,[],"ret",[]));
    state.setInstructions([nop0,eicall,nop1,ret]);
    state.zreg.value = cast(ushort)(0x1001);
    state.setIoRegisterByIo(0x3c, cast(ubyte)(0x21));

    ushort spInit = state.stackPointer.value;

    state.fetchInstruction().callback(state); //nop
    auto instr = state.fetchInstruction();
    assert(instr == eicall);
    eicall.callback(state);

    assert(state.stackPointer.value == spInit - 3);
    assert(state.nextInstruction.address == 0x211001);
    assert(state.data[spInit-2] == 2);
}

class Elpm : Instruction!AvrState {
    uint regd;
    bool postinc;

    this(in InstructionToken token) {
        super(token);
        enforce(token.parameters[1][0] == 'Z', "Elpm works on the Z register");
        if(token.parameters[1].length == 2) {
            postinc = token.parameters[1][1] == '+';
        }

        regd = parseNumericRegister(token.parameters[0]);

        enforce(!((regd == 30 && postinc) || (regd == 31 && postinc)),
                "Undefined behavior");
    }

    override cycleCount callback(AvrState state) const {
        size_t z = cast(size_t)(state.zreg);
        size_t rampz = cast(size_t)(state.getIoRegisterByIo(0x3b));

        size_t offset = (z & 0x0001);

        size_t address = (z & 0x00ffff) + ((rampz & 0x00ffff) << 16);
        ubyte value = state.program[address + (offset == 0 ? 1 : 0)];

        state.valueRegisters[regd] = value;

        if(postinc) {
            address = (address + 1) & 0xffffff;
        }

        state.setIoRegisterByIo(0x3b, cast(ubyte)(address >> 16));
        state.zreg.value = cast(ushort)(address & 0x00ffff);
        return 3;
    }
}

unittest {
    auto state = new AvrState();
    auto elpm = new Elpm(new InstructionToken(0,0,[], "elpm", ["r0", "Z+"]));
    auto elpm2 = new Elpm(new InstructionToken(0,2,[], "elpm", ["r1", "Z"]));

    state.setInstructions([elpm, elpm2]);
    state.setIoRegisterByIo(0x3b, 0x01);
    state.zreg.value = 0x1000;
    state.program[0x011000] = 0xaa;
    state.program[0x011000 + 1] = 0xbb;

    state.fetchInstruction();
    elpm.callback(state);
    assert(state.valueRegisters[0].value == 0xbb);
    assert(state.zreg.value == 0x1001);
    state.fetchInstruction();
    elpm2.callback(state);
    assert(state.valueRegisters[1].value = 0xaa);
}


/* Exclusive OR */
class Eor : Instruction!AvrState {
    uint regd;
    uint regr;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
        regr = parseNumericRegister(token.parameters[1]);
    }

    override cycleCount callback(AvrState state) const {
        ubyte rd = state.valueRegisters[regd].value;
        ubyte rr = state.valueRegisters[regr].value;
        ubyte result = rr ^ rd;
        state.valueRegisters[regd].value = result;
        state.setSregLogical(result);
        return 1;
    }
}

class Jmp : Instruction!AvrState {
    size_t dest;

    this(in InstructionToken token) {
        super(token);
        dest = parseHex(token.parameters[0]);
        assert(dest < 4*1024*1024);
    }

    override void optimize(in InstructionsWrapper!AvrState iw) {
        dest = iw.getInstructionIndex(dest);
    }

    override cycleCount callback(AvrState state) const {
        state.jumpIndex(dest);
        return 3;
    }
}

class In : Instruction!AvrState {
    uint regd;
    size_t ioAddr;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
        ioAddr = parseHex(token.parameters[1]);
    }

    override cycleCount callback(AvrState state) const {
        state.valueRegisters[regd].value = state.getIoRegisterByIo(ioAddr).value;
        return 1;
    }
}

unittest {
    auto state = new AvrState();
    auto in1 = new In(new InstructionToken(0,0,[], "in", ["r0", "0x3b"]));

    state.setInstructions([in1]);
    state.setIoRegisterByIo(0x3b, 0xab);

    state.fetchInstruction();
    in1.callback(state);
    assert(state.valueRegisters[0].value == 0xab);
}

class Inc : Instruction!AvrState {
    uint reg;

    this(in InstructionToken token) {
        super(token);
        reg = parseNumericRegister(token.parameters[0]);
    }

    override cycleCount callback(AvrState state) const {
        ubyte rd = state.valueRegisters[reg].value;
        ubyte result = cast(ubyte)(rd + 1);
        state.valueRegisters[reg].value = result;

        state.sreg.V = rd == 0x7f;
        state.sreg.N = cast(bool)(result & 0x80);
        state.sreg.S = state.sreg.N ^ state.sreg.V;
        state.sreg.Z = result == 0;

        return 1;
    }
}

class Ld : Instruction!AvrState {
    bool predec, postinc;
    uint regd;
    string refreg;

    this(in InstructionToken token) {
        super(token);
        if(token.parameters[1].length == 2) {
            predec = token.parameters[1][0] == '-';
            postinc = token.parameters[1][1] == '+';
            if (predec) {
                refreg = token.parameters[1][1 .. 2].dup;
            }
            if (postinc) {
                refreg = token.parameters[1][0 .. 1].dup;
            }
        } else {
                refreg = token.parameters[1][0 .. 1].dup;
        }

        regd = parseNumericRegister(token.parameters[0]);
    }

    override cycleCount callback(AvrState state) const {
        if(predec) {
            state.refregs[refreg].value = cast(ushort)(state.refregs[refreg].value - 1);
        }

        size_t addr = state.refregs[refreg].value;
        state.valueRegisters[regd].value = state.data[addr];

        if(postinc) {
            state.refregs[refreg].value = cast(ushort)(state.refregs[refreg].value + 1);
        }
        // todo: cycles can differ
        return 2;
    }
}

class Ldd : Instruction!AvrState {
    uint q;
    uint regd;
    string refreg;

    this(in InstructionToken token) {
        super(token);
        enforce(token.parameters[1][1] == '+');
        q = parseHex(token.parameters[1][2..$]);
        refreg = token.parameters[1][0 .. 1].dup;
        regd = parseNumericRegister(token.parameters[0]);
    }

    override cycleCount callback(AvrState state) const {
        size_t addr = state.refregs[refreg].value + q;
        //stderr.writefln("addr: %x, q: %d", addr, q);
        state.valueRegisters[regd].value = state.data[addr];
        return 2;
    }
}

/* Load immediate */
class Ldi : Instruction!AvrState {
    uint regd;
    uint k;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
        k = parseHex(token.parameters[1]);
    }

    override cycleCount callback(AvrState state) const {
        state.valueRegisters[regd].value = cast(ubyte)k;
        return 1;
    }
}

/* Load direct from data space */
class Lds : Instruction!AvrState {
    uint regd;
    uint address;

    this(in InstructionToken token) {
      super(token);
      regd = parseNumericRegister(token.parameters[0]);
      address = parseHex(token.parameters[1]);
    }

    override cycleCount callback(AvrState state) const {
      state.valueRegisters[regd].value = state.data[address];
      return 2;
    }
}

class Lpm : Instruction!AvrState {
    uint regd;
    bool postinc;

    this(in InstructionToken token) {
        super(token);
        enforce(token.parameters[1][0] == 'Z', "Elpm works on the Z register");
        if(token.parameters[1].length == 2) {
            postinc = token.parameters[1][1] == '+';
        }

        regd = parseNumericRegister(token.parameters[0]);

        enforce(!((regd == 30 && postinc) || (regd == 31 && postinc)),
                "Undefined behavior");
    }

    override cycleCount callback(AvrState state) const {
        ushort z = state.zreg.value;
        ushort offset = (z & 0x0001);

        ubyte value = state.program[z + (offset == 0 ? 1 : 0)];

        state.valueRegisters[regd].value = value;

        if(postinc) {
            z = cast(ushort)(z + 1);
        }

        state.zreg.value = z;
        return 3;
    }
}

class Lsr : Instruction!AvrState {
    uint regd;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
    }

    override cycleCount callback(AvrState state) const {
        ubyte rd = state.valueRegisters[regd].value;
        ubyte result = cast(ubyte)(rd >> 1);
        state.valueRegisters[regd].value = result;

        state.sreg.N = false;
        state.sreg.C = (rd & 0x01) == 1;
        state.sreg.V = state.sreg.N ^ state.sreg.C;
        state.sreg.S = state.sreg.N ^ state.sreg.V;
        state.sreg.Z = result == 0;

        return 1;
    }
}

/* Store register to I/O location */
/* TODO, but not necessarily for the first sprint target */
class Out : Instruction!AvrState {
    this(in InstructionToken token) { super(token); }

    override cycleCount callback(AvrState state) const { return 1; }
}

/* Store indirect from register to data space using index */
class St : Instruction!AvrState {
    bool preinc, predec, postinc, postdec; //Probably not the most beautiful way...
    string refreg;
    uint regr;

    this(in InstructionToken token) {
      super(token);
      if(token.parameters[0].length == 2) { //TODO: 'parsing' has to happen somewhere else, but is this general enough for base.d?
        preinc = token.parameters[0][0] == '+';
        predec = token.parameters[0][0] == '-';
        if(preinc || predec)
          refreg = token.parameters[0][1..$].dup;
        else {
          postinc = token.parameters[0][1] == '+';
          postdec = token.parameters[0][1] == '-';
          refreg = token.parameters[0][0..1].dup;
        }
      }
      else
        refreg = token.parameters[0];

      regr = parseNumericRegister(token.parameters[1]);
    }

    override cycleCount callback(AvrState state) const {
      if(preinc)
        state.refregs[refreg].value = cast(ushort)(state.refregs[refreg].value + 1);
      if(predec)
        state.refregs[refreg].value = cast(ushort)(state.refregs[refreg].value - 1);

      ushort addr = state.refregs[refreg];
      enforce(addr < state.data.size, format("address: %x", addr));
      state.data[addr] = state.valueRegisters[regr].value;

      if(postinc)
        state.refregs[refreg].value = cast(ushort)(state.refregs[refreg].value + 1);
      if(postdec)
        state.refregs[refreg].value = cast(ushort)(state.refregs[refreg].value - 1);
      return 1;
    }
}

unittest {
    auto state = new AvrState();
    auto st = new St(new InstructionToken(0, 0, [], "st", ["Z", "r0"]));
    state.setInstructions([st]);

    state.valueRegisters[30] = 0xab;
    state.valueRegisters[31] = 0x10;

    assert(state.zreg.value == 0x10ab);

    //state.zreg = 0x10ab;

    state.valueRegisters[0] = 0xdf;

    state.fetchInstruction().callback(state);

    writeln("state: %x", state.data[0x10ab]);

    assert(state.data[0x10ab] == 0xdf);
}

/* Store direct to data space */
class Sts : Instruction!AvrState {
    uint address;
    uint regr;

    this(in InstructionToken token) {
      super(token);
      address = parseHex(token.parameters[0]);
      regr = parseNumericRegister(token.parameters[1]);
    }

    override cycleCount callback(AvrState state) const {
      state.data[address] = state.valueRegisters[regr].value;
      return 2;
    }
}

class Nop : Instruction!AvrState {
    this(in InstructionToken token) {
        super(token);
    }

    override cycleCount callback(AvrState state) const {
        return 1;
    }
}

class Ret : Instruction!AvrState {
    this(in InstructionToken token) {
        super(token);
    }

    override cycleCount callback(AvrState state) const {
        uint newPc = state.data[state.stackPointer + 1] +
            (state.data[state.stackPointer + 2] << 8)+
            (state.data[state.stackPointer + 3] << 16);

        state.stackPointer.value = cast(ushort)(state.stackPointer.value + 3);
        state.jump(newPc * 2);

        return 5;
    }
}

class Rjmp : Instruction!AvrState {
    size_t dest;

    this(in InstructionToken token) {
        super(token);
        int jumpOffset = parseInt(token.parameters[0]);
        // *2 since the instruction set manual specifies k in words and we use
        // bytes
        assert(jumpOffset <= 2000*2);
        assert(-2000*2 <= jumpOffset);
        dest = this.address + 2 + jumpOffset;
    }

    override void optimize(in InstructionsWrapper!AvrState instructions) {
        dest = instructions.getInstructionIndex(dest);
    }

    override cycleCount callback(AvrState state) const {
        state.jumpIndex(dest);
        return 2;
    }
}
unittest {
    auto state = new AvrState();
    auto rjmp = new Rjmp(new InstructionToken(0,0,[],"rjmp",[".+2"]));
    auto nop1 = new Nop(new InstructionToken(0,2,[],"nop1",[]));
    auto rjmp2 = new Rjmp(new InstructionToken(0,4,[],"rjmp",[".-4"]));
    state.setInstructions([rjmp,nop1,rjmp2]);

    state.fetchInstruction(); // fetch rjmp
    auto cycles = rjmp.callback(state);
    assert(cycles == 2);
    assert(state.fetchInstruction().address == 4);
    rjmp2.callback(state);
    assert(state.fetchInstruction().address == 2);
}

class Mov : Instruction!AvrState {
    uint regd;
    uint regr;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
        regr = parseNumericRegister(token.parameters[1]);
    }

    override cycleCount callback(AvrState state) const {
        state.valueRegisters[regd].value = state.valueRegisters[regr].value;
        return 1;
    }
}
unittest {
    auto state = new AvrState();
    state.valueRegisters[5].value = 14;
    state.valueRegisters[3].value = 0;
    auto mov = new Mov(new InstructionToken(0, 0, [], "mov", ["r3", "r5"]));
    auto cycles = mov.callback(state);
    assert(cycles == 1);
    assert(state.valueRegisters[3].value == 14);
}

class Movw : Instruction!AvrState {
    uint regd;
    uint regr;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
        regr = parseNumericRegister(token.parameters[1]);
    }

    override cycleCount callback(AvrState state) const {
        state.valueRegisters[regd].value = state.valueRegisters[regr].value;
        state.valueRegisters[regd+1].value = state.valueRegisters[regr+1].value;
        return 1;
    }
}
unittest {
    auto state = new AvrState();
    state.valueRegisters[5].value = 14;
    state.valueRegisters[6].value = 7;
    state.valueRegisters[3].value = 0;
    state.valueRegisters[4].value = 0;
    auto movw = new Movw(new InstructionToken(0, 0, [], "movw", ["r3", "r5"]));
    auto cycles = movw.callback(state);
    assert(cycles == 1);
    assert(state.valueRegisters[3].value == 14);
    assert(state.valueRegisters[4].value == 7);
}

class Mul : Instruction!AvrState {
    uint regd;
    uint regr;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
        regr = parseNumericRegister(token.parameters[1]);
    }

    override cycleCount callback(AvrState state) const {
        ushort rd = state.valueRegisters[regd].value;
        ushort rr = state.valueRegisters[regr].value;
        int result = rr * rd;
        state.valueRegisters[1].value = cast(ubyte)(result >>> 8);
        state.valueRegisters[0].value = cast(ubyte)(result & 0xff);
        state.sreg.Z = result == 0;
        state.sreg.C = cast(bool)(result & 0x8000);
        return 2;
    }
}
unittest {
    auto state = new AvrState();
    state.valueRegisters[3] = 201;
    state.valueRegisters[5] = 211;
    auto mul = new Mul(new InstructionToken(0, 0, [], "mul", ["r3", "r5"]));
    auto cycles = mul.callback(state);
    assert(cycles == 2);
    assert(state.valueRegisters[0].value == 0xab);
    assert(state.valueRegisters[1].value == 0xa5);
    assert(!state.sreg.Z);
    assert(state.sreg.C);

    state.valueRegisters[3].value = 0;
    cycles = mul.callback(state);
    assert(cycles == 2);
    assert(state.valueRegisters[0].value == 0);
    assert(state.valueRegisters[1].value == 0);
    assert(state.sreg.Z);
    assert(!state.sreg.C);
}

class Neg : Instruction!AvrState {
    uint regd;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
    }

    override cycleCount callback(AvrState state) const {
        ubyte rd = state.valueRegisters[regd].value;
        ubyte result = cast(ubyte)(256 - rd);
        state.valueRegisters[regd].value = cast(ubyte)(result);
        state.sreg.H = cast(bool)(result & 0b00001000 | rd ^ 0b00001000);
        state.sreg.V = result == 0x80;
        state.sreg.N = cast(bool)(result & 0b10000000);
        state.sreg.S = state.sreg.V ^ state.sreg.N;
        state.sreg.Z = result == 0;
        state.sreg.C = result != 0;
        return 1;
    }
}
unittest {
    auto state = new AvrState();
    state.valueRegisters[1] = 0x62;
    auto neg = new Neg(new InstructionToken(0, 0, [], "neg", ["r1"]));
    auto cycles = neg.callback(state);
    assert(cycles == 1);
    assert(state.valueRegisters[1].value == 0x9e);
    assert(state.sreg.H);
    assert(!state.sreg.V);
    assert(state.sreg.N);
    assert(state.sreg.S);
    assert(!state.sreg.Z);
    assert(state.sreg.C);

    state.valueRegisters[1] = 0x80;
    cycles = neg.callback(state);
    assert(cycles == 1);
    assert(state.valueRegisters[1].value == 0x80);
    assert(state.sreg.H);
    assert(state.sreg.V);
    assert(state.sreg.N);
    assert(!state.sreg.S);
    assert(!state.sreg.Z);
    assert(state.sreg.C);
}

class Or : Instruction!AvrState {
    uint regd;
    uint regr;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
        regr = parseNumericRegister(token.parameters[1]);
    }

    override cycleCount callback(AvrState state) const {
        ubyte rd = state.valueRegisters[regd].value;
        ubyte rr = state.valueRegisters[regr].value;
        ubyte result = rd | rr;
        state.valueRegisters[regd].value = result;
        state.setSregLogical(result);
        return 1;
    }
}
unittest {
    auto state = new AvrState();
    state.valueRegisters[1] = 0x62;
    state.valueRegisters[2] = 0xf0;
    auto or = new Or(new InstructionToken(0, 0, [], "or", ["r1", "r2"]));
    auto cycles = or.callback(state);
    assert(cycles == 1);
    assert(state.valueRegisters[1].value == 0xf2);
    assert(!state.sreg.V);
    assert(state.sreg.N);
    assert(state.sreg.S);
    assert(!state.sreg.Z);
}

class Ori : Instruction!AvrState {
    uint regd;
    ubyte k;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
        k = cast(ubyte)(parseHex(token.parameters[1]));
    }

    override cycleCount callback(AvrState state) const {
        ubyte rd = state.valueRegisters[regd].value;
        ubyte result = rd | k;
        state.valueRegisters[regd].value = result;
        state.setSregLogical(result);
        return 1;
    }
}
unittest {
    auto state = new AvrState();
    state.valueRegisters[1] = 0x38;
    auto ori = new Ori(new InstructionToken(0, 0, [], "ori", ["r1", "0x49"]));
    auto cycles = ori.callback(state);
    assert(cycles == 1);
    assert(state.valueRegisters[1].value == 0x79);
    assert(!state.sreg.V);
    assert(!state.sreg.N);
    assert(!state.sreg.S);
    assert(!state.sreg.Z);
}

class Pop : Instruction!AvrState {
    uint regd;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
    }

    override cycleCount callback(AvrState state) const {
        state.stackPointer.value = cast(ushort)(state.stackPointer.value + 1);
        state.valueRegisters[regd].value = state.data[state.stackPointer.value];
        return 2;
    }
}

class Push : Instruction!AvrState {
    uint regd;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
    }

    override cycleCount callback(AvrState state) const {
        state.data[state.stackPointer.value] = state.valueRegisters[regd].value;
        state.stackPointer.value = cast(ushort)(state.stackPointer.value - 1);
        return 2;
    }
}
unittest {
    auto state = new AvrState();
    state.valueRegisters[0] = 42;
    auto push = new Push(new InstructionToken(0, 0, [], "push", ["r0"]));
    auto cycles = push.callback(state);
    assert(cycles == 2);
    assert(state.stackPointer.value == 8701);
    assert(state.data[8702] == 42);
    auto pop = new Pop(new InstructionToken(0, 0, [], "push", ["r1"]));
    cycles = pop.callback(state);
    assert(cycles == 2);
    assert(state.stackPointer.value == 8702);
    assert(state.valueRegisters[1].value == 42);
}

class Rcall : Instruction!AvrState {
    size_t dest;

    this(in InstructionToken token) {
        super(token);
        //+2 since this instruction is 2 bytes
        dest = address + 2 + cast(ushort)(parseInt(token.parameters[0]));
    }

    override void optimize(in InstructionsWrapper!AvrState iw) {
        dest = iw.getInstructionIndex(dest);
    }

    override cycleCount callback(AvrState state) const {
        size_t pc = state.programCounter;
        state.data[state.stackPointer.value -2 .. state.stackPointer.value+1] =
            [cast(ubyte)(pc), cast(ubyte)(pc >>> 8), cast(ubyte)(pc >>> 16)];
        state.stackPointer.value = cast(ushort)(state.stackPointer.value - 3);

        state.jumpIndex(dest);

        return 4;
    }
}

unittest {
    auto state = new AvrState();
    auto nop0 = new Nop(new InstructionToken(0,0,[],"nop",[]));
    //rcall jumps to the ret instruction
    auto rcall = new Rcall(new InstructionToken(0,2,[],"rcall",[".+2"]));
    auto nop1 = new Nop(new InstructionToken(0,4,[],"nop",[]));
    auto ret = new Ret(new InstructionToken(0,6,[],"ret",[]));
    state.setInstructions([nop0,rcall,nop1,ret]);

    ushort spInit = state.stackPointer.value;

    state.fetchInstruction().callback(state); //nop
    auto instr = state.fetchInstruction();
    assert(instr == rcall);
    rcall.callback(state);

    assert(state.stackPointer.value == spInit - 3);
    assert(state.programCounter == 3);
    assert(state.data[spInit-2] == 2);
}

class Ror : Instruction!AvrState {
    uint regd;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
    }

    override cycleCount callback(AvrState state) const {
        ubyte rd = state.valueRegisters[regd].value;
        ubyte result = rd >>> 1 | (state.sreg.C ? 0x80 : 0);
        state.valueRegisters[regd].value = result;
        state.sreg.N = cast(bool)(result & 0b10000000);
        state.sreg.Z = result == 0;
        state.sreg.C = rd & 1;
        state.sreg.V = state.sreg.N ^ state.sreg.C;
        state.sreg.S = state.sreg.V ^ state.sreg.N;
        return 1;
    }
}
unittest {
    auto state = new AvrState();
    state.sreg.C = false;
    state.valueRegisters[1].value = 0b00101101;
    auto ror = new Ror(new InstructionToken(0,0,[],"ror",["r1"]));
    auto cycles = ror.callback(state);
    assert(cycles == 1);
    assert(state.valueRegisters[1].value == 0b00010110);
    assert(!state.sreg.N);
    assert(!state.sreg.Z);
    assert(state.sreg.C);
    assert(state.sreg.V);
    assert(state.sreg.S);
    cycles = ror.callback(state);
    assert(cycles == 1);
    assert(state.valueRegisters[1].value == 0b10001011);
    assert(state.sreg.N);
    assert(!state.sreg.Z);
    assert(!state.sreg.C);
    assert(state.sreg.V);
    assert(!state.sreg.S);
}

class Sbc : Instruction!AvrState {
    uint regd;
    uint regr;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
        regr = parseNumericRegister(token.parameters[1]);
    }

    override cycleCount callback(AvrState state) const {
        ubyte rd = state.valueRegisters[regd].value;
        ubyte rr = state.valueRegisters[regr].value;
        ubyte result = cast(ubyte)(rd - rr - state.sreg.C);
        state.valueRegisters[regd].value = result;
        state.setSregArithNeg(rd, rr, result);

        return 1;
    }
}
unittest {
    auto state = new AvrState();
    state.sreg.C = false;
    state.valueRegisters[2] = 8;
    state.valueRegisters[0] = 15;
    auto sbc = new Sbc(new InstructionToken(0,0,[],"sbc",["r2", "r0"]));
    auto cycles = sbc.callback(state);
    assert(cycles == 1);
    assert(state.valueRegisters[2].value == 249);
    assert(state.sreg.C);
    sbc.callback(state);
    assert(state.valueRegisters[2].value == 233);
    assert(!state.sreg.C);
}

class Sbci : Instruction!AvrState {
    uint regd;
    ubyte k;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
        k = cast(ubyte)(parseHex(token.parameters[1]));
    }

    override cycleCount callback(AvrState state) const {
        ubyte rd = state.valueRegisters[regd].value;
        ubyte result = cast(ubyte)(rd - k - state.sreg.C);
        state.valueRegisters[regd].value = result;
        state.setSregArithNeg(rd, k, result);

        return 1;
    }
}
unittest {
    auto state = new AvrState();
    state.sreg.C = false;
    state.valueRegisters[2] = 8;
    auto sbci = new Sbci(new InstructionToken(0,0,[],"sbc",["r2", "15"]));
    auto cycles = sbci.callback(state);
    assert(cycles == 1);
    assert(state.valueRegisters[2].value == 249);
    assert(state.sreg.C);
    sbci.callback(state);
    assert(state.valueRegisters[2].value == 233);
    assert(!state.sreg.C);
}

class Sbiw : Instruction!AvrState {
    uint regd;
    ubyte k;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
        k = cast(ubyte)(parseHex(token.parameters[1]));
    }

    override cycleCount callback(AvrState state) const {
        ushort rd = state.valueRegisters[regd + 1].value << 8 | state.valueRegisters[regd].value;
        ushort result = cast(ushort)(rd - k);
        state.valueRegisters[regd + 1] = cast(ubyte)(result >>> 8);
        state.valueRegisters[regd].value = cast(ubyte)(result);

        state.sreg.V = cast(bool)(rd & (result ^ 0x8000) & 0x8000);
        state.sreg.N = cast(bool)(result & 0x8000);
        state.sreg.Z = result == 0;
        state.sreg.C = cast(bool)(result & (rd ^ 0x8000) & 0x8000);
        state.sreg.S = state.sreg.V ^ state.sreg.N;

        return 2;
    }
}
unittest {
    auto state = new AvrState();
    state.valueRegisters[24] = 0x13;
    state.valueRegisters[25] = 0x30;
    auto sbiw = new Sbiw(new InstructionToken(0,0,[],"sbc",["r24", "0x15"]));
    auto cycles = sbiw.callback(state);
    assert(cycles == 2);
    assert(state.valueRegisters[24].value == 0xfe);
    assert(state.valueRegisters[25].value == 0x2f);
    assert(!state.sreg.V);
    assert(!state.sreg.N);
    assert(!state.sreg.Z);
    assert(!state.sreg.C);
    assert(!state.sreg.S);
}

class Set : Instruction!AvrState {
    this(in InstructionToken token) {
        super(token);
    }

    override cycleCount callback(AvrState state) const {
        state.sreg.T = 1;
        return 1;
    }
}
unittest {
    auto state = new AvrState();
    state.sreg.T = false;
    auto set = new Set(new InstructionToken(0,0,[],"set",[]));
    auto cycles = set.callback(state);
    assert(cycles == 1);
    assert(state.sreg.T);
}

class Std : Instruction!AvrState {
    string refreg;
    uint q;
    uint regr;

    this(in InstructionToken token) {
        super(token);
        refreg = token.parameters[0][0..1].dup;
        assert(token.parameters[0][1] == '+');
        q = parseHex(token.parameters[0][2..$]);
        regr = parseNumericRegister(token.parameters[1]);
    }

    override cycleCount callback(AvrState state) const {
      state.data[state.refregs[refreg] + q] = state.valueRegisters[regr].value;
      return 2;
    }
}
unittest {
    auto state = new AvrState();
    state.zreg = 0x0242;
    state.valueRegisters[0] = 42;
    auto std = new Std(new InstructionToken(0,0,[],"std",["Z+8", "r0"]));
    auto cycles = std.callback(state);
    assert(cycles == 2);
    assert(state.data[0x024A] == 42);
}

class Sub : Instruction!AvrState {
    uint regd;
    uint regr;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
        regr = parseNumericRegister(token.parameters[1]);
    }

    override cycleCount callback(AvrState state) const {
        ubyte rd = state.valueRegisters[regd].value;
        ubyte rr = state.valueRegisters[regr].value;
        ubyte result = cast(ubyte)(rd - rr);
        state.valueRegisters[regd].value = result;
        state.setSregArithNeg(rd, rr, result);
        state.sreg.Z = result == 0;

        return 1;
    }
}
unittest {
    auto state = new AvrState();
    state.valueRegisters[2] = 8;
    state.valueRegisters[0] = 15;
    auto sub = new Sub(new InstructionToken(0,0,[],"sub",["r2", "r0"]));
    auto cycles = sub.callback(state);
    assert(cycles == 1);
    assert(state.valueRegisters[2].value == 249);
    assert(state.sreg.C);
    sub.callback(state);
    assert(state.valueRegisters[2].value == 234);
    assert(!state.sreg.C);
}

class Subi : Instruction!AvrState {
    uint regd;
    ubyte k;

    this(in InstructionToken token) {
        super(token);
        regd = parseNumericRegister(token.parameters[0]);
        k = cast(ubyte)(parseHex(token.parameters[1]));
    }

    override cycleCount callback(AvrState state) const {
        ubyte rd = state.valueRegisters[regd].value;
        ubyte result = cast(ubyte)(rd - k);
        state.valueRegisters[regd].value = result;
        state.setSregArithNeg(rd, k, result);
        state.sreg.Z = result == 0;

        return 1;
    }
}
unittest {
    auto state = new AvrState();
    state.valueRegisters[2] = 8;
    auto subi = new Subi(new InstructionToken(0,0,[],"subi",["r2", "15"]));
    auto cycles = subi.callback(state);
    assert(cycles == 1);
    assert(state.valueRegisters[2].value == 249);
    assert(state.sreg.C);
    subi.callback(state);
    assert(state.valueRegisters[2].value == 234);
    assert(!state.sreg.C);
}

abstract class SkipInstruction :Instruction!AvrState {
    this(in InstructionToken token) {super(token);}

    bool shouldSkip(AvrState state) const;

    override cycleCount callback(AvrState state) const {
        if(shouldSkip(state)) {
            state.relativeJump(2);
            //Calculate difference between addresses of next 2 instructions
            //+2 since this instruction is 2 bytes
            size_t skipSize = state.nextInstruction.address - (address + 2);

            if(skipSize == 2) {
                return 2;
            } else {
                assert(skipSize == 4);
                return 3;
            }
        } else {
            return 1;
        }
    }
}

/** SBRS - Skip if Bit in Register is Set */
class Sbrs : SkipInstruction {
    uint register;
    int bit;

    this(in InstructionToken token) {
        super(token);
        register = parseNumericRegister(token.parameters[0]);
        bit = parseInt(token.parameters[1]);
        assert(0 <= bit && bit <= 7);
    }

    override bool shouldSkip(AvrState state) const {
        size_t regValue = state.valueRegisters[register].value;
        return bt(&regValue,bit) > 0;
    }
}

class Sbrc : SkipInstruction {
    uint register;
    int bit;

    this(in InstructionToken token) {
        super(token);
        register = parseNumericRegister(token.parameters[0]);
        bit = parseInt(token.parameters[1]);
        assert(0 <= bit && bit <= 7);
    }

    override bool shouldSkip(AvrState state) const {
        size_t regValue = state.valueRegisters[register].value;
        return bt(&regValue,bit) == 0;
    }
}

class WriteByte : Instruction!AvrState {
    this(in InstructionToken tok) {
        super(tok);
    }

    override cycleCount callback(AvrState state) const {
        auto value = state.valueRegisters[24].bytes;
        write(cast(string)(value));
        stdout.flush();
        return 0;
    }
}

unittest {
    auto state = new AvrState();
    state.valueRegisters[0].value = 0;
    auto sbrs = new Sbrs(new InstructionToken(0,0,[],"sbrs",["r0","0"]));
    auto nop1 = new Nop(new InstructionToken(0,2,[],"nop1",[]));
    auto nop2 = new Nop(new InstructionToken(0,4,[],"nop2",[]));
    state.setInstructions([sbrs,nop1,nop2]);

    //Should not have skipped instruction
    state.fetchInstruction();
    assert(sbrs.callback(state) == 1);
    assert(state.programCounter == 1);

    state.jump(0);
    state.valueRegisters[0] = 1;
    //Should skip instruction
    assert(sbrs.callback(state) == 2);
    assert(state.programCounter == 2);

    //Test skipping 4byte instruction with bit 7
    sbrs = new Sbrs(new InstructionToken(0,0,[],"sbrs",["r0","7"]));
    auto call = new Call(new InstructionToken(0,2,[],"call",["0x0"]));
    nop1 = new Nop(new InstructionToken(0,6,[],"nop",[]));
    state.setInstructions([sbrs,call,nop1]);

    //Should skip instruction
    state.valueRegisters[0] = 0xf0;
    assert(sbrs.callback(state) == 3);
    assert(state.programCounter == 3);
}

abstract class AvrFactory : MachineFactory {
    static Instruction!AvrState createInstruction(AvrState)(in InstructionToken tok) {
        switch (tok.name) {
            case "add" : return new Add(tok);
            case "adc" : return new Adc(tok);
            case "adiw": return new Adiw(tok);
            case "andi": return new Andi(tok);
            case "and": return new And(tok);
            case "brcc": return new Brcc(tok);
            case "brcs": return new Brcs(tok);
            case "breq": return new Breq(tok);
            case "brge": return new Brge(tok);
            case "brne": return new Brne(tok);
            case "brtc": return new Brtc(tok);
            case "brlt": return new Brlt(tok);
            case "call": return new Call(tok);
            case "cli": return new Cli(tok);
            case "clt": return new Clt(tok);
            case "com": return new Com(tok);
            case "cp": return new Cp(tok);
            case "cpc": return new Cpc(tok);
            case "cpi": return new Cpi(tok);
            case "cpse": return new Cpse(tok);
            case "dec": return new Dec(tok);
            case "eicall": return new Eicall(tok);
            case "elpm": return new Elpm(tok);
            case "eor": return new Eor(tok);
            case "jmp": return new Jmp(tok);
            case "in": return new In(tok);
            case "inc": return new Inc(tok);
            case "ld": return new Ld(tok);
            case "ldd": return new Ldd(tok);
            case "ldi": return new Ldi(tok);
            case "lds": return new Lds(tok);
            case "lpm": return new Lpm(tok);
            case "lsr": return new Lsr(tok);
            case "out": return new Out(tok);
            case "st": return new St(tok);
            case "sts": return new Sts(tok);
            case "nop": return new Nop(tok);
            case "ret": return new Ret(tok);
            case "rjmp": return new Rjmp(tok);
            case "mov": return new Mov(tok);
            case "movw": return new Movw(tok);
            case "mul": return new Mul(tok);
            case "neg": return new Neg(tok);
            case "or": return new Or(tok);
            case "ori": return new Ori(tok);
            case "pop": return new Pop(tok);
            case "push": return new Push(tok);
            case "rcall": return new Rcall(tok);
            case "ror": return new Ror(tok);
            case "sbc": return new Sbc(tok);
            case "sbci": return new Sbci(tok);
            case "sbiw": return new Sbiw(tok);
            case "set": return new Set(tok);
            case "std": return new Std(tok);
            case "sub": return new Sub(tok);
            case "subi": return new Subi(tok);
            case "sbrs": return new Sbrs(tok);
            case "sbrc": return new Sbrc(tok);
            case "write_byte": return new WriteByte(tok);
            default: throw new Exception("Unknown instruction: " ~ tok.name);
        }
    }

    static Instruction!AvrState[] createInstructions(in InstructionToken[] tokens) {
        Instruction!AvrState[] instructions = [];
        foreach (tok; tokens) {
            instructions ~= createInstruction!AvrState(tok);
        }
        return instructions;
    }

    static Memory fillProgramMemory(in InstructionToken[] tokens, Memory
            programMemory) {
        foreach(tok; tokens) {
            programMemory[tok.address .. tok.address + tok.raw.length] = tok.raw;
        }
        return programMemory;
    }
}
