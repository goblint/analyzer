// configuration
#define nproc 3 // number of processes
#define nsema 2 // number of semaphores

// setup arinc functions and resources
#include "arinc_base.pml"

// init
init {
    preInit;
    run mainfun(0); // creates resources
    postInit(); // blocks until PartitionMode == NORMAL
    run monitor();
    run a(1);
    run b(2);
}

/* ltl pw1 { ! (eventually always (status[1] == WAITING)) } */
/* ltl pw2 { ! (eventually always (status[2] == WAITING)) } */
/* ltl ps1 { ! (eventually always (status[1] == SUSPENDED)) } */
/* ltl ps2 { ! (eventually always (status[2] == SUSPENDED)) } */

proctype mainfun(byte id) provided canRun(0) {
    CreateSemaphore(0, 1, 1, FIFO);
    CreateSemaphore(1, 1, 1, FIFO);
	CreateProcess(1, 53, 4294967295, 4294967295); // a (prio 53, period ∞, capacity ∞)
	CreateProcess(2, 53, 4294967295, 4294967295); // a (prio 53, period ∞, capacity ∞)
    Start(1);
    Start(2);
	SetPartitionMode(NORMAL);
}

proctype a(byte id) provided canRun(1) {
    WaitSemaphore(0);
    WaitSemaphore(1);
    printf("Process a has both locks!\n");
    SignalSemaphore(1);
    SignalSemaphore(0);
}

proctype b(byte id) provided canRun(2) {
    WaitSemaphore(1);
    WaitSemaphore(0);
    printf("Process b has both locks!\n");
    SignalSemaphore(0);
    SignalSemaphore(1);
}

