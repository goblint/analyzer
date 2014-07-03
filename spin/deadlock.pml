// configuration
#define nproc 3 // number of processes
#define nsema 2 // number of semaphores
#define PREEMPTION 1

// macro to check the status of all processes (derived macros defined in base)
#define checkStatus(op1, v, op2) (status[0] op1 v op2 status[1] op1 v op2 status[2] op1 v)

// setup arinc functions and resources
#include "arinc.base.pml"

// init
init {
    // preInit;
    status[0] = RUNNING;
    run mainfun(0); // creates resources
    postInit(); // blocks until PartitionMode == NORMAL
    run monitor();
    run scheduler();
    run a(1);
    run b(2);
}

ltl pw { ! (eventually always oneIs(WAITING)) }
ltl ps { ! (eventually always oneIs(SUSPENDED)) }
// starvation: process will always be READY but never RUNNING
ltl pr { ! (eventually always oneIs(READY)) }

#define PRIO0
#define PRIO1
#define PRIO2
#ifdef PRIOS
// here executability constraints arising from the priorities can be redefined for each process
// e.g. let a have a higher prio than b, then b can only run if a is not READY:
/* #define PRIO2 && status[1] != READY */
#endif


proctype scheduler() provided (partitionMode == NORMAL && (PREEMPTION || noneAre(RUNNING))) { atomic {
    // find one process that can run (chooses non-determ. if multiple could run)
    byte p;
    do ::
        #if PREEMPTION
        // we need to make sure that another process ran before the next scheduling takes place
        (_last != _pid);
        #endif
        if // encode priority here :)
        :: canRun(0) PRIO0 -> p = 0
        :: canRun(1) PRIO1 -> p = 1
        :: canRun(2) PRIO2 -> p = 2
        :: allAre(DONE) -> printf("All done :)\n"); break
        :: else ->
            if
            :: nperiodicWait > 0 -> periodicWake() // we can still wake up some waiting processes
            :: else -> printf("No process can run but not all are done :(\n"); break
            fi
        fi
        printf("The scheduler selected process %d to be RUNNING!\n", p);
        #if PREEMPTION
        // there might be a process running -> set it to ready
        changeStatus(RUNNING, READY);
        #else
        // no processes running
        // but: if some process did a PeriodicWait, we have to wake it again at some point (non-determ., but at the latest when no other process can run (see else-case above))
        if
        :: periodicWake()
        :: skip
        fi
        #endif
        status[p] = RUNNING;
    od
} }

proctype mainfun(byte id) provided isRunning(0) {
    CreateSemaphore(0, 1, 1, FIFO);
    CreateSemaphore(1, 1, 1, FIFO);
	CreateProcess(1, 53, 4294967295, 4294967295); // a (prio 53, period ∞, capacity ∞)
	CreateProcess(2, 53, 4294967295, 4294967295); // a (prio 53, period ∞, capacity ∞)
    Start(1);
    Start(2);
	SetPartitionMode(NORMAL);
    status[id] = DONE;
}

proctype a(byte id) provided isRunning(1) {
    WaitSemaphore(0);
    // PeriodicWait(); // needed for deadlock if no PREEMPTION
    WaitSemaphore(1);
    printf("Process a has both locks!\n");
    SignalSemaphore(1);
    SignalSemaphore(0);
    status[id] = DONE;
}

proctype b(byte id) provided isRunning(2) {
    WaitSemaphore(1);
    WaitSemaphore(0);
    printf("Process b has both locks!\n");
    SignalSemaphore(0);
    SignalSemaphore(1);
    status[id] = DONE;
}

