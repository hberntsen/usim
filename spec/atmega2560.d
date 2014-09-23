module spec.atmega2560; //we might want to make this a more generic AVR later on

import std.stdio : writeln;
import std.conv;

import spec.base;
import machine.state;

class AtMega2560State : MachineState {
    Memory data;
    Memory program;
    Memory eeprom;

    Instruction[ulong] instructions;

    ReferenceRegister!(ubyte)[32] valueRegisters;

    this() {
        data = new Memory(8 * 1024 + 512, 0);
        program = new Memory(256 * 1024, 0);
        eeprom = new Memory(4 * 1024, 0);

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

    cycleCount apply(Instruction instruction) {
        return instruction.callback(this);
    }
}

class Adc : Instruction {

    uint dest;
    uint toAdd;

    string name = "adi";

    this(in string[] parameters) {
        assert(parameters[0][0] == 'r');
        string r = parameters[0][1 .. $];
        dest = to!uint(r);
        toAdd = to!uint(parameters[1]);
    }

    override cycleCount callback(inout MachineState state) {
        AtMega2560State st = cast(AtMega2560State)state;

        st.valueRegisters[dest].bytes[0] +=  cast(ubyte)toAdd;
        return 1;
    }
}

void main()
{
    auto adc = new Adc(["r1", "0x05"]);
    auto mem = new Memory(32, 0);
    auto register = new ReferenceRegister!ubyte("r2", 1, mem);

    //writeln(register.bytes);
    //register.bytes = [0x02];
    //writeln(register.bytes);

    //writeln(mem.data);
    //writeln(mem[1]);

    auto state = new AtMega2560State();
    state.apply(adc);
    writeln(state.data[0 .. 32]);
}
