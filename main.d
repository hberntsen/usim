#! /usr/bin/env rdmd
import core.stdc.stdio;
import core.stdc.stdlib;
import core.sys.posix.dlfcn;
import machine.state;

void main() 
{
    printf("+main()\n");

    void* lh = dlopen("libatmega2560.so", RTLD_LAZY);
    if (!lh)
    {
        fprintf(stderr, "dlopen error: %s\n", dlerror());
        exit(1);
    }
    printf("libatmega2560.so is loaded\n");

    MachineState function() fn = cast(MachineState function())dlsym(lh, "getNewMachineState");
    char* error = dlerror();
    if (error)
    {
        fprintf(stderr, "dlsym error: %s\n", error);
        exit(1);
    }
    printf("getNewMachineState() function is found\n");

    MachineState state = fn();
    writeln("Registers:");
    writeln(state.registers);
    writeln("Memories");
    writeln(state.memories);

    printf("unloading libdll.so\n");
    dlclose(lh);

    printf("-main()\n");
}
