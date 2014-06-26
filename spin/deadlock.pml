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

ltl pw { ! (eventually always (status[0] == WAITING || status[1] == WAITING || status[2] == WAITING)) }
ltl ps { ! (eventually always (status[0] == SUSPENDED || status[1] == SUSPENDED || status[2] == SUSPENDED)) }

#define PRIO0
#define PRIO1
#define PRIO2
#ifdef PRIO
// here executability constraints arising from the priorities can be redefined for each process
// e.g. let a have a higher prio than b, then b can only run if a is not READY:
#define PRIO2 && status[1] != READY
#endif

proctype mainfun(byte id) provided (canRun(0) PRIO0) {
    CreateSemaphore(0, 1, 1, FIFO);
    CreateSemaphore(1, 1, 1, FIFO);
	CreateProcess(1, 53, 4294967295, 4294967295); // a (prio 53, period ∞, capacity ∞)
	CreateProcess(2, 53, 4294967295, 4294967295); // a (prio 53, period ∞, capacity ∞)
    Start(1);
    Start(2);
	SetPartitionMode(NORMAL);
    status[id] = DONE;
}

proctype a(byte id) provided (canRun(1) PRIO1) {
    WaitSemaphore(0);
    WaitSemaphore(1);
    printf("Process a has both locks!\n");
    SignalSemaphore(1);
    SignalSemaphore(0);
    status[id] = DONE;
}

proctype b(byte id) provided (canRun(2) PRIO2) {
    WaitSemaphore(1);
    WaitSemaphore(0);
    printf("Process b has both locks!\n");
    SignalSemaphore(0);
    SignalSemaphore(1);
    status[id] = DONE;
}

