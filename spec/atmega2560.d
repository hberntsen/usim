module spec.atmega2560;

import parser.parser;
import machine.state;
import spec.avrstate;
import std.exception;


static this() {
    enum AvrChipSpec chip = AvrChipSpec();
    machineFactories["atmega2560"] = new AvrFactory!chip();
}
