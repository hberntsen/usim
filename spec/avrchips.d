module spec.avrchips;

import machine.state;
import spec.avrstate;

static this() {
    enum AvrChipSpec atmega2560 = AvrChipSpec();
    machineFactories["atmega2560"] = new AvrFactory!atmega2560();

    enum AvrChipSpec atmega88 = {
        dataSize: 1024+512,
        programSize: 8*1024,
        eepromSize: 512
    };
    machineFactories["atmega88"] = new AvrFactory!atmega2560();

    enum AvrChipSpec attiny10 = {
        chipType: AvrChipSpec.ChipType.REDUCED_CORE,
        dataSize: 96,
        programSize: 1024,
        //Should be a 0, but ldc2 has some problems with that:
        //https://github.com/ldc-developers/ldc/issues/812
        eepromSize: 1,
        sregOffset: 0x3f,
        spOffset: 0x3d,
        valueRegistersInDataMemory: false,
        UDR: 0x04
    };
    machineFactories["attiny10"] = new AvrFactory!attiny10();
}
