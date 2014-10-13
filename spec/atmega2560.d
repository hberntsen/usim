module spec.atmega2560; //we might want to make this a more generic AVR later on

import std.stdio;
import std.conv;
import std.system;
import std.string;

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
    ReferenceRegister!ushort xreg, yreg, zreg;
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
        xreg = new ReferenceRegister!ushort("X", 26, data);
        yreg = new ReferenceRegister!ushort("Y", 28, data);
        zreg = new ReferenceRegister!ushort("Z", 30, data);
        //The stack pointer's initial value points to the end of the internal
        //SRAM: 8703
        stackPointer = new ReferenceRegister!ushort("SP",cast(size_t)0x5d, data);
        stackPointer.value = cast(ushort)(data.size - 2);

        for(int i = 0; i < valueRegisters.length; i++) {
            valueRegisters[i] = new ReferenceRegister!ubyte("r" ~ i.stringof, i, data);
        }
    }

    @property ReferenceRegister!ushort[string] refregs() {
        return ["X": xreg, "Y": yreg, "Z": zreg];
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

    Instruction!AtMega2560State fetchInstruction() {
        return instructions.fetch();
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

    private static bool[] getRelevantBits(ubyte a, ubyte b, ubyte c) {
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

/** Branch If Not Equal */
class Brne : Instruction!AtMega2560State {
    const size_t destAddress;

    this(in InstructionToken token) {
        super(token);
        auto relativePc = parseInt(token.parameters[0]);
        assert(relativePc <= 63 && relativePc >= -64);
        destAddress = address + 2 + relativePc;
    }

    override cycleCount callback(AtMega2560State state) const {
        if(state.sreg.Z) {
            return 1;
        } else {
            state.jump(destAddress);
            return 2;
        }
    }
}
unittest {
    auto state = new AtMega2560State();
    state.sreg.Z = false;
    //Jump to nop2 if Z is cleared (false)
    auto brne = new Brne(new InstructionToken(0,0,[],"brne",[".+2"]));
    auto nop = new Nop(new InstructionToken(0,2,[],"nop",[]));
    auto nop2 = new Nop(new InstructionToken(0,4,[],"nop",[]));
    state.setInstructions([brne,nop,nop2]);
    brne.callback(state);
    assert(state.currentInstruction().address == 4);

    state.sreg.Z = true;
    state.programCounter = 0;
    writeln(state.sreg);
    brne.callback(state);
    assert(state.fetchInstruction().address == 2);
}

/** Long Call to a Subroutine */
class Call : Instruction!AtMega2560State {
    const ulong address;

    this(in InstructionToken token) {
        super(token);
        address = parseHex(token.parameters[0]);
    }

    override cycleCount callback(AtMega2560State state) const {
        ubyte[] pcBytes = new ubyte[size_t.sizeof]; 
        //Convert PC+2 to bytes and store it on the stack
        pcBytes.write!(size_t,Endian.littleEndian)(state.programCounter+2,0);
        state.data[state.stackPointer.value -2 .. state.stackPointer.value+1] =
            pcBytes[0 .. 3];
        state.stackPointer.value = cast(ushort)(state.stackPointer.value - 3);

        state.jump(address);

        return 5;
    }
}
///Tests both call and ret
unittest {
    auto state = new AtMega2560State();
    auto nop0 = new Nop(new InstructionToken(0,0,[],"nop0",[]));
    //call jumps to the ret instruction 
    auto call = new Call(new InstructionToken(0,2,[],"call",["0x8"]));
    auto nop1 = new Nop(new InstructionToken(0,6,[],"nop",[]));
    auto ret = new Ret(new InstructionToken(0,8,[],"ret",[]));
    state.setInstructions([nop0,call,nop1,ret]);
    ushort spInit = state.stackPointer.value;
    nop0.callback(state);
    call.callback(state);

    assert(state.stackPointer.value == spInit - 3);
    //Program counter that is stored should be 3, the program counter is 1
    //before call is executed, then raised to a value of 3 since the call
    //instruction takes 4 bytes 
    assert(state.programCounter == 4);
    assert(state.data[spInit-2] == 3);

    //Done testing call, now test ret
    ret.callback(state);
    assert(state.programCounter == 3);
    assert(state.stackPointer.value == spInit);
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

/** Compare Skip if Equal */
class Cpse : SkipInstruction {
    uint rd;
    uint rr;

    this(in InstructionToken token) {
        super(token);
        rd = parseNumericRegister(token.parameters[0]);
        rr = parseNumericRegister(token.parameters[1]);
    }

    override bool shouldSkip(AtMega2560State state) const {
        return state.valueRegisters[rd].value ==
            state.valueRegisters[rr].value;
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

class Jmp : Instruction!AtMega2560State {
    const ulong address;

    this(in InstructionToken token) {
        super(token);
        address = parseHex(token.parameters[0]);
        assert(address < 4*1024*1024);
    }

    override cycleCount callback(AtMega2560State state) const {
        state.jump(address);
        return 3;
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

/* Load direct from data space */
class Lds : Instruction!AtMega2560State {
    uint regd;
    uint address;

    this(in InstructionToken token) {
      super(token);
      regd = parseNumericRegister(token.parameters[0]);
      address = parseHex(token.parameters[1]);
    }

    override cycleCount callback(AtMega2560State state) const {
      state.valueRegisters[regd].value = state.memories["data"][address];
      return 2;
    }
}

/* Store register to I/O location */
/* TODO, but not necessarily for the first sprint target */
class Out : Instruction!AtMega2560State {
    this(in InstructionToken token) { super(token); }

    override cycleCount callback(AtMega2560State state) const { return 1; }
}

/* Store indirect from register to data space using index */
class St : Instruction!AtMega2560State {
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

    override cycleCount callback(AtMega2560State state) const {
      if(preinc)
        state.refregs[refreg].value = cast(ushort)(state.refregs[refreg].value + 1);
      if(predec)
        state.refregs[refreg].value = cast(ushort)(state.refregs[refreg].value - 1);

      state.memories["data"][state.refregs[refreg]] = state.valueRegisters[regr].value;

      if(postinc)
        state.refregs[refreg].value = cast(ushort)(state.refregs[refreg].value + 1);
      if(postdec)
        state.refregs[refreg].value = cast(ushort)(state.refregs[refreg].value - 1);
      return 1;
    }
}

/* Store direct to data space */
class Sts : Instruction!AtMega2560State {
    uint address;
    uint regr;

    this(in InstructionToken token) {
      super(token);
      address = parseHex(token.parameters[0]);
      regr = parseNumericRegister(token.parameters[1]);
    }

    override cycleCount callback(AtMega2560State state) const {
      state.memories["data"][address] = state.valueRegisters[regr].value;
      return 2;
    }
}

class Nop : Instruction!AtMega2560State {
    this(in InstructionToken token) { super(token);}

    override cycleCount callback(AtMega2560State state) const {
        state.relativeJump(1);
        return 1;
    }
}

class Ret : Instruction!AtMega2560State {
    this(in InstructionToken token) {
        super(token);
    }

    override cycleCount callback(AtMega2560State state) const {
        //Since we use uint for the stackpointer and the hardware's program
        //counter is only 3 bytes we need to copy the bytes first to a new
        //array and then convert it
        ubyte[] stackPointerBytes = new ubyte[uint.sizeof];
        //Stack pointer points to next position to write, so +1 and +5 since we
        //want 3 bytes
        stackPointerBytes[0 .. 4] = state.data[state.stackPointer +1 ..
            state.stackPointer+5];
        uint newPc = stackPointerBytes.peek!(uint,Endian.littleEndian);

        state.stackPointer.value = cast(short)(state.stackPointer.value + 3);
        state.programCounter = newPc;
        return 5;
    }
}

class Rjmp : Instruction!AtMega2560State {
    const size_t destAddress;

    this(in InstructionToken token) {
        super(token);
        int jumpOffset = parseInt(token.parameters[0]);
        assert(jumpOffset <= 2000);
        assert(-2000 <= jumpOffset);
        destAddress = this.address + 2 + jumpOffset;
    }

    override cycleCount callback(AtMega2560State state) const {
        state.jump(destAddress);
        return 2;
    }
}
unittest {
    auto state = new AtMega2560State();
    auto rjmp = new Rjmp(new InstructionToken(0,0,[],"rjmp",[".+2"]));
    auto nop1 = new Nop(new InstructionToken(0,2,[],"nop1",[]));
    auto rjmp2 = new Rjmp(new InstructionToken(0,4,[],"rjmp",[".-4"]));
    state.setInstructions([rjmp,nop1,rjmp2]);
    auto cycles = rjmp.callback(state);
    assert(cycles == 2);
    assert(state.currentInstruction.address == 4);
    rjmp2.callback(state);
    assert(state.currentInstruction.address == 2);
}

abstract class SkipInstruction :Instruction!AtMega2560State {
    this(in InstructionToken token) {super(token);}

    bool shouldSkip(AtMega2560State state) const;

    override cycleCount callback(AtMega2560State state) const {
        if(shouldSkip(state)) {
            state.relativeJump(2);
            //Calculate difference between addresses of next 2 instructions
            //+2 since this instruction is 2 bytes
            ulong skipSize = state.currentInstruction.address - (address + 2);

            if(skipSize == 2) {
                return 2;
            } else {
                assert(skipSize == 4);
                return 3;
            }
        } else {
            state.relativeJump(1);
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

    override bool shouldSkip(AtMega2560State state) const {
        ulong regValue = state.valueRegisters[register].value;
        return bt(&regValue,bit) > 0;
    }
}

class WriteByte : Instruction!AtMega2560State {
    this(in InstructionToken tok) {
        super(tok);
    }

    override cycleCount callback(AtMega2560State state) const {
        auto value = state.valueRegisters[24].bytes;
        write(cast(string)(value));
        stdout.flush();
        return 0;
    }
}

unittest {
    auto state = new AtMega2560State();
    state.valueRegisters[0].value = 0;
    auto sbrs = new Sbrs(new InstructionToken(0,0,[],"sbrs",["r0","0"]));
    auto nop1 = new Nop(new InstructionToken(0,2,[],"nop1",[]));
    auto nop2 = new Nop(new InstructionToken(0,4,[],"nop2",[]));
    state.setInstructions([sbrs,nop1,nop2]);

    //Should not have skipped instruction
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
