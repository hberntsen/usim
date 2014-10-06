module machine.state;

import std.stdio : writeln;
import std.conv;
import std.bitmanip;
import std.system;

abstract class Register
{
    string _name;

    @property string name() {
        return name;
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
    Memory raw;
    ulong offset;

    override @property ubyte[] bytes() {
        return raw[offset .. offset + T.sizeof];
    };

    @property ubyte[] bytes() const {
        return raw[offset .. offset + T.sizeof].dup;
    };

    override @property ubyte[] bytes(ubyte[] newValue) {
        assert(newValue.length == T.sizeof);
        auto slice = raw[offset .. offset + T.sizeof];
        slice[] = newValue;
        return slice;
    }

    alias value this;

    @property T value() const {
        return peek!(T,Endian.littleEndian)(bytes);
    }
    
    @property T value(T newValue) {
        bytes().write!(T,Endian.littleEndian)(newValue,0);
        return newValue;
    }

    this(in string name, in ulong offset, Memory raw) {
        super(name);
        this.offset = offset;
        this.raw = raw;
    }
}

//Test the ReferenceRegister implementation with a stack example
unittest {
    Memory data = new Memory(8*1024+512,0);
    ulong stackPointerLocation = 0x5d;
    auto stackPointer = new ReferenceRegister!ushort("SP",cast(ulong)0x5d, data);
    stackPointer.value = 123;
    assert(stackPointer.value == 123);
    stackPointer.value = 8703;// equals 0b1111111100100001 or cast(ushort)(data.size - 2);
    assert(stackPointer.value == 8703);
    assert(data[stackPointerLocation] == 0xFF); //Low byte
    assert(data[stackPointerLocation+1] == 0b00100001); //High byte
    assert(stackPointer.value == 8703);
}

class Memory {
    ubyte[] data;
    ulong offset;

    @property size_t size() { return data.length;}

    this(ulong size, ulong offset =0) {
        this.offset = offset;
        this.data = new ubyte[size];
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

    ubyte[] opSlice(size_t i1, size_t i2) const {
        return data[i1 - offset .. i2 - offset].dup;
    }
    
    ubyte[] opSlice(size_t i1, size_t i2) {
        return data[i1 - offset .. i2 - offset];
    }
}

interface MachineState {
    @property Register[] registers();
    @property Memory[string]  memories();

}
