// configuration
#define nproc 2 // number of processes
#define nsema 2 // number of semaphores

// setup arinc functions and resources
#include "arinc.pml"

// init
init {
    init_sema(0, 1, 1);
    init_sema(1, 1, 1);
    run monitor();
    run a(0);
    run b(1);
}

proctype a(byte id) provided (status[0] == READY) {
    WaitSema(0);
    WaitSema(1);
    printf("Process a has both locks!\n");
    SignalSema(1);
    SignalSema(0);
}

proctype b(byte id) provided (status[1] == READY) {
    WaitSema(1);
    WaitSema(0);
    printf("Process b has both locks!\n");
    SignalSema(0);
    SignalSema(1);
}

