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
        return *cast(ubyte[T.sizeof]*)&value;
    }

    override @property ubyte[] bytes(ubyte[] newValue) {
        value = *cast(T*)&newValue;
        return newValue;
    }

    this(in string name, in T value) {
        super(name);
        this.value = value;
    }
}

class ReferenceRegister(T) : Register {
    private T* valuePointer;
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
            return *valuePointer;
        }

        @property T value(T newValue) {
            *valuePointer = newValue;
            return newValue;
        }
    }

    this(in string name, in size_t offset, Memory raw) {
        super(name);
        slice2 = raw[offset .. offset + T.sizeof];
        valuePointer = cast(T*)&slice2[0];
    }

    this(in string name, in size_t offset, ubyte[] raw) {
        super(name);
        slice2 = raw[offset .. offset + T.sizeof];
        valuePointer = cast(T*)&slice2[0];
    }
}

//Test the ReferenceRegister implementation with a stack example
unittest {
    Memory data = new Memory(8*1024+512,0);
    size_t stackPointerLocation = 0x5d;
    auto stackPointer = new ReferenceRegister!ushort("SP",cast(size_t)0x5d, data);
    stackPointer.value = 123;
    assert(stackPointer.value == 123);
    stackPointer.value = 8703;// equals 0b1111111100100001 or cast(ushort)(data_.size - 2);
    assert(stackPointer.value == 8703);
    assert(data[stackPointerLocation] == 0xFF); //Low byte
    assert(data[stackPointerLocation+1] == 0b00100001); //High byte
    assert(stackPointer.value == 8703);
}

final class Memory {
    ubyte[] data_;
    size_t offset;
    @property ubyte[] data() { return data_; }

    @property size_t size() { return data_.length;}

    size_t opDollar() {
        return size + offset;
    }

    this(size_t size, size_t offset =0) {
        this.offset = offset;
        this.data_ = new ubyte[size];
    }

    this(ubyte[] data_, size_t offset=0) {
        this.data_ = data_;
    }

    override string toString() {
        return format("%s", data_);
    }

    ubyte opIndex(size_t i) {
        return data_[i - offset];
    }

    ubyte opIndexAssign(ubyte value, size_t i) {
        data_[i - offset] = value;
        return value;
    }

    Memory opAssign(ubyte[] value) {
        data_[offset .. value.length + offset] = value;
        return this;
    }

    void opSliceAssign(in ubyte[] value, size_t i, size_t j) {
        data_[i - offset .. j -offset] = value;
    }

    const(ubyte[]) opSlice(size_t i1, size_t i2) const {
        return data_[i1 - offset .. i2 - offset];
    }

    ubyte[] opSlice(size_t i1, size_t i2) {
        return data_[i1 - offset .. i2 - offset];
    }
}

interface MachineState {
    @property size_t programCounter();
    @property size_t programCounter(size_t newpc);

    @property Register[] registers();
    @property Memory[string]  memories();

    void jump(size_t address);
    void relativeJump(in int instructionOffset);

    void update(cycleCount cycles);

    // todo: stack operations
}

