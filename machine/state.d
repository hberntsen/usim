module machine.state;

import std.stdio : writeln;
import std.conv;
import std.bitmanip;
import std.system;
import std.string;
import spec.base;
import parser.parser : InstructionToken;
import simulator.simulator;

abstract class Register
{
    string _name;
    bool reverse = false;

    @property string name() {
        return name;
    }

    override string toString() {
        return format("0x%(%02x%)", reverse ? bytes.dup.reverse : bytes);
    }


    abstract @property ubyte[] bytes();
    abstract @property ubyte[] bytes(ubyte[] newValue);

    this(in string name) {
        this._name = name;
    }
}

class SimpleRegister(T) : Register
{
    T value;

    override @property ubyte[] bytes() {
        //todo: convert to ubytes
        return new ubyte[0];
    }
    //return to!ubyte[](value); }
    override @property ubyte[] bytes(ubyte[] newValue) {
    //    value = newValue.read!T();
    //todo: some magic to convert the ubytes to our T
        return newValue;
    }

    this(in string name, in T value) {
        super(name);
        this.value = value;
    }
}

class ReferenceRegister(T) : Register {
    private ubyte[] slice2;

    //TODO: somehow does not compile
    //invariant() {
        //The slice should always be a shared slice
        //assert(this.slice2.capacity == 0);
    //}

    final {
        override @property ubyte[] bytes() {
            return slice2;
        };

        @property const(ubyte[]) bytes() const {
            return slice2;
        }

        override @property ubyte[] bytes(ubyte[] newValue) {
            assert(newValue.length <= slice2.length);
            slice2[] = newValue;
            assert(this.slice2.capacity == 0);
            return slice2;
        }

        alias value this;

        @property T value() const {
            return *cast(T*)&slice2[0];
        }

        @property T value(T newValue) {
            *cast(T*)&slice2[0] = newValue;
            return newValue;
        }
    }

    this(in string name, in size_t offset, Memory raw) {
        super(name);
        slice2 = raw[offset .. offset + T.sizeof];
    }
}

//Test the ReferenceRegister implementation with a stack example
unittest {
    Memory data = new Memory(8*1024+512,0);
    size_t stackPointerLocation = 0x5d;
    auto stackPointer = new ReferenceRegister!ushort("SP",cast(size_t)0x5d, data);
    stackPointer.value = 123;
    assert(stackPointer.value == 123);
    stackPointer.value = 8703;// equals 0b1111111100100001 or cast(ushort)(data.size - 2);
    assert(stackPointer.value == 8703);
    assert(data[stackPointerLocation] == 0xFF); //Low byte
    assert(data[stackPointerLocation+1] == 0b00100001); //High byte
    assert(stackPointer.value == 8703);
}

final class Memory {
    ubyte[] data;
    size_t offset;

    @property size_t size() { return data.length;}
    
    size_t opDollar() {
        return size + offset;
    }

    this(size_t size, size_t offset =0) {
        this.offset = offset;
        this.data = new ubyte[size];
    }

    override string toString() {
        return format("%s", data);
    }

    ubyte opIndex(size_t i) {
        return data[i - offset];
    }

    ubyte opIndexAssign(ubyte value, size_t i) {
        data[i - offset] = value;
        return value;
    }

    void opSliceAssign(in ubyte[] value, size_t i, size_t j) {
        data[i - offset .. j -offset] = value;
    }

    const(ubyte[]) opSlice(size_t i1, size_t i2) const {
        return data[i1 - offset .. i2 - offset];
    }

    ubyte[] opSlice(size_t i1, size_t i2) {
        return data[i1 - offset .. i2 - offset];
    }
}

interface MachineState {
    @property size_t programCounter();
    @property size_t programCounter(size_t newpc);

    @property Register[] registers();
    @property Memory[string]  memories();

    void jump(size_t address);
    void relativeJump(in int instructionOffset);

    // todo: stack operations
}

abstract class MachineFactory {
    public MachineState createState(in InstructionToken[] token, in ubyte[] data) const;
    public BatchModeSimulator createBatchModeSimulator(in InstructionToken[] token, in ubyte[] data) const;
}

MachineFactory[string] machineFactories;
