// configuration
#define nproc 2 // number of processes
#define nsema 1 // number of semaphores
#define nevent 1 // number of events
// handles for processes (for Suspend/Resume...)
#define dd63_id 2

// setup arinc functions and resources
#include "arinc.pml"

// init
init {
    /* status[0] = RUNNING; */
    semas[0] = 1; // initial count
    semas_max[0] = 1; // max count
    run monitor();
    run a(0);
    run b(1);
    /* run acem(0); */
    /* run ctl_dd63(1); */
    /* run dd63(2); */
}

proctype a(byte id) priority 5 provided (status[0] == READY) { WaitSignalSema(0); }
proctype b(byte id) priority 5 provided (status[1] == READY) { WaitSignalSema(0); }

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

