// configuration
#define nproc 2 // number of processes
#define nsema 2 // number of semaphores

// setup arinc functions and resources
#include "arinc_base.pml"

// init
init {
    CreateSemaphore(0, 1, 1, FIFO);
    CreateSemaphore(1, 1, 1, FIFO);
    Start(0);
    Start(1);
    SetPartitionMode(NORMAL); // arinc init process not needed
    run monitor();
    run a(0);
    run b(1);
}

proctype a(byte id) provided canRun(0) {
    WaitSemaphore(0);
    WaitSemaphore(1);
    printf("Process a has both locks!\n");
    SignalSemaphore(1);
    SignalSemaphore(0);
}

proctype b(byte id) provided canRun(1) {
    WaitSemaphore(1);
    WaitSemaphore(0);
    printf("Process b has both locks!\n");
    SignalSemaphore(0);
    SignalSemaphore(1);
}

