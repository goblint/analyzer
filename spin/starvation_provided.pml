// configuration
#define nproc 3 // number of processes

// process status macros
#define oneIs(mode) (status[0] == mode || status[1] == mode || status[2] == mode)
#define allAre(mode) (status[0] == mode && status[1] == mode && status[2] == mode)
#define noneAre(mode) (status[0] != mode && status[1] != mode && status[2] != mode)

// setup arinc functions and resources
#include "arinc_base.pml"

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
// if a keeps running, b will starve and never finish
ltl starve2 { ! (eventually always (status[2] == READY)) }
ltl starve2d { eventually (status[2] == DONE) }

#define PRIO0
#define PRIO1
#define PRIO2
#ifdef PRIOS
// here executability constraints arising from the priorities can be redefined for each process
// e.g. let a have a higher prio than b, then b can only run if a is not READY:
/* #define PRIO2 && status[1] != READY */
#endif

proctype mainfun(byte id) provided (canRun(0) PRIO0) {
	CreateProcess(1, 53, 4294967295, 4294967295); // a (prio 53, period ∞, capacity ∞)
	CreateProcess(2, 53, 4294967295, 4294967295); // a (prio 53, period ∞, capacity ∞)
    Start(1);
    Start(2);
	SetPartitionMode(NORMAL);
    status[id] = DONE;
}

proctype a(byte id) provided (canRun(1) PRIO1) {
    foo: PeriodicWait(); goto foo
    status[id] = DONE;
}

proctype b(byte id) provided (canRun(2) PRIO2) {
    PeriodicWait();
    status[id] = DONE;
}

