LDC2 = /usr/bin/ldc2

SOURCES = machine/state.d main.d parser/parser.d simulator/simulator.d simulator/debugger.d
SOURCES += spec/avrchips.d spec/avrstate.d spec/base.d machine/factory.d

usim: ${SOURCES}
	$(LDC2) -O5 -release -singleobj -of=usim ${SOURCES}
