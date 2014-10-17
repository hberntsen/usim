build:
	dmd -c spec/atmega2560.d -fPIC
	dmd -oflibatmega2560.so atmega2560.o -shared -defaultlib=libphobos2.so -L-rpath=.
	dmd -c main.d
	dmd main.o machine/state.d spec/base.d parser/parser.d -L-ldl -defaultlib=libphobos2.so -L-rpath=.

clean:
	rm *.o
	rm *.so
	rm *.map
	rm main
