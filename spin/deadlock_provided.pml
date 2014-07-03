// configuration
#define nproc 3 // number of processes
#define nsema 2 // number of semaphores

// macro to check the status of all processes (derived macros defined in base)
#define checkStatus(op1, v, op2) (status[0] op1 v op2 status[1] op1 v op2 status[2] op1 v)

// setup arinc functions and resources
#include "arinc.base.pml"

// init
init {
    // preInit;
    status[0] = READY;
    run mainfun(0); // creates resources
    postInit(); // blocks until PartitionMode == NORMAL
    run monitor();
    run a(1);
    run b(2);
}

ltl pw { ! (eventually always oneIs(WAITING)) }
ltl ps { ! (eventually always oneIs(SUSPENDED)) }
ltl pr { ! (eventually always oneIs(READY)) }

#define PRIO0
#define PRIO1
#define PRIO2
#ifdef PRIOS
// here executability constraints arising from the priorities can be redefined for each process
// e.g. let a have a higher prio than b, then b can only run if a is not READY:
/* #define PRIO2 && status[1] != READY */
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

