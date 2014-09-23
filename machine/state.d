module machine.state;

import std.stdio : writeln;
import std.conv;
import std.bitmanip;

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

    @property T value() const {
        ubyte[] buffer = bytes();
        return buffer.read!T();
    }
    
    @property T value(T newValue) {
        bytes().write(newValue,0);
        return newValue;
    }

    this(in string name, in ulong offset, Memory raw) {
        super(name);
        this.offset = offset;
        this.raw = raw;
    }
}

class Memory {
    ubyte[] data;
    ulong offset;

    this(ulong size, ulong offset =0) {
        this.offset = offset;
        this.data = new ubyte[size];
    }

    ubyte opIndex(size_t i) {
        return data[i - offset];
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
