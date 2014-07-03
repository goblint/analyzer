// configuration
#define nproc 3 // number of processes
#define nsema 1 // number of semaphores
#define nevent 1 // number of events

// setup arinc functions and resources
#include "arinc.base.pml"

// init
init {
    Start(0);
    run arinc_init(0); // process with id 0 is the arinc init process!
    (partitionMode == NORMAL); // blocks until mode is NORMAL
    run monitor(); // checks system invariants
    // activate processes
    run a(1);
    run b(2);
    /* run acem(0); */
    /* run ctl_dd63(1); */
    /* run dd63(2); */
}

proctype arinc_init(byte id) provided canRun(0) {
    CreateSemaphore(0, 1, 1, FIFO); // id, current count, max count
    Start(1);
    Start(2);
    SetPartitionMode(NORMAL);
}
proctype a(byte id) priority 5 provided canRun(1) { WaitSignalSema(0); }
proctype b(byte id) priority 5 provided canRun(2) { WaitSignalSema(0); }

/* // define processes */
/* proctype acem(byte id) { // prio 60, per 600 */
/*     SetEvent(0); */
/*     do :: */
/*         WaitSignalSema(0); */
/*     od */
/* } */
/* // serv_nvm: prio 55, per 600 */
/* // init prio 53, per inf */
/* proctype ctl_dd63(byte id) { // prio 15, per 600 */
/*     SetEvent(0); */
/*     do :: */
/*         Resume(dd63_id); */
/*         WaitEvent(0); */
/*         ResetEvent(0); */
/*         Suspend(dd63_id); */
/*     od */
/* } */
/* proctype dd63(byte id) { // prio 10, per inf */
/*     printf("\ndd63 pid is %d\n\n", _pid); */
/*     Suspend(dd63_id); */
/*     do :: */
/*         WaitSignalSema(0); */
/*         SetEvent(0); */
/*     od */
/* } */

