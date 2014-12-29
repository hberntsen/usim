LDC2 = /usr/bin/ldc2

SOURCES = machine/state.d main.d parser/parser.d simulator/instruction.d
SOURCES += simulator/simulator.d spec/avrchips.d spec/avrstate.d spec/base.d
SOURCES += machine/factory.d

usim: ${SOURCES}
	$(LDC2) -O5 -release -singleobj -of=usim ${SOURCES}
