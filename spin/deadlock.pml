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
    run scheduler();
    run a(1);
    run b(2);
}

ltl pw { ! (eventually always (status[0] == WAITING || status[1] == WAITING || status[2] == WAITING)) }
ltl ps { ! (eventually always (status[0] == SUSPENDED || status[1] == SUSPENDED || status[2] == SUSPENDED)) }
// starvation: process will always be READY but never RUNNING
ltl pr { ! (eventually always (status[0] == READY || status[1] == READY || status[2] == READY)) }

#define PRIO0
#define PRIO1
#define PRIO2
#ifdef PRIOS
// here executability constraints arising from the priorities can be redefined for each process
// e.g. let a have a higher prio than b, then b can only run if a is not READY:
/* #define PRIO2 && status[1] != READY */
#endif

#define allAre(mode) (status[0] == mode && status[1] == mode && status[2] == mode)
#define noneAre(mode) (status[0] != mode && status[1] != mode && status[2] != mode)

#define PREEMPTION 1

proctype scheduler() provided (partitionMode == NORMAL && (PREEMPTION || noneAre(RUNNING))) { atomic {
    // find one process that can run (non-determ.)
    byte p;
    do ::
        if // encode priority here :)
        :: canRun(0) PRIO0 -> p = 0
        :: canRun(1) PRIO1 -> p = 1
        :: canRun(2) PRIO2 -> p = 2
        :: allAre(DONE) -> printf("All done :)\n"); break
        :: else -> printf("No process can run but not all are done :(\n"); assert(false)
        fi
        printf("The scheduler selected process %d to be RUNNING!\n", p);
        #if PREEMPTION
        changeStatus(RUNNING, READY);
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

