// configuration
#define nproc 3 // number of processes
#define nsema 1 // number of semaphores
#define nevent 1 // number of events

#define checkStatus(op1, v, op2) (status[0] op1 v op2 status[1] op1 v op2 status[2] op1 v)

// setup arinc functions and resources
#include "arinc.base.pml"

// init
init {
    preInit;
    run mainfun(0);
    postInit();
    run monitor();
    // activate processes
    run a(1);
    run b(2);
}

proctype mainfun(byte id) provided canRun(0, 0) {
    CreateSemaphore(0, 1, 1, FIFO); // id, current count, max count
    Start(1);
    Start(2);
    SetPartitionMode(NORMAL);
}

proctype f1(byte id, byte caller_id) priority 5 provided canRun(1, 1) {
    WaitSignalSema(0);
    fun[id] = caller_id;
}
proctype f2(byte id, byte caller_id) priority 5 provided canRun(2, 2) {
    WaitSignalSema(0);
    fun[id] = caller_id;
}

/* proctype t1(byte id) priority 5 provided canRun(1) { WaitSignalSema(0); } */
/* proctype t2(byte id) priority 5 provided canRun(2) { WaitSignalSema(0); } */
proctype t1(byte id) priority 5 provided canRun(1, 0) {
    call f1;
}
proctype t2(byte id) priority 5 provided canRun(2, 0) {
    call f2;
}
