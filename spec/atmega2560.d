module spec.atmega2560; //we might want to make this a more generic AVR later on

import parser.parser;
import machine.state;
import spec.avrstate;
import std.exception;

class AtMega2560State : AvrState {
    this() {
        super(
            8 * 1024 + 512, // data
            256 * 1024, // program
            4 * 1024 // eeprom
        );
    }
}

class AtMega2560Factory : AvrFactory {
    static this() {
        machineFactories["atmega2560"] = new this();
    }

    override MachineState createState(in InstructionToken[] tokens, in ubyte[] data) const {
        auto state = new AtMega2560State();
        enum AvrChipSpec chip = AvrChipSpec(256*1024/2, AvrChipSpec.ChipType.OTHER);
        state.setInstructions(createInstructions!chip(tokens));
        enforce(data.length <= state.program.size);
        state.program[0 .. data.length] = data;
        return state;
    }
}

