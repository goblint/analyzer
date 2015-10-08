// configuration
#define nproc 3 // number of processes
#define nsema 1 // number of semaphores

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
    run t1(1);
    run t2(2);
}

proctype mainfun(byte id) provided canRun(0) {
    CreateSemaphore(0, 1, 1, FIFO); // id, current count, max count
    CreateProcess(1, 53, 4294967295, 4294967295); // a (prio 53, period ∞, capacity ∞)
    CreateProcess(2, 53, 4294967295, 4294967295); // a (prio 53, period ∞, capacity ∞)
    Start(1);
    Start(2);
    SetPartitionMode(NORMAL);
}

proctype f1(byte id; int fun_id) provided canRun(1) {
    WaitSemaphore(0);
    ret_fun();
}
proctype f2(byte id; int fun_id) provided canRun(2) {
    call_fun(f3, 3);
    ret_fun();
}
proctype f3(byte id; int fun_id) provided canRun(2) {
    WaitSemaphore(0);
    ret_fun();
}

proctype t1(byte id) priority 5 provided canRun(1) {
    call_fun(f1, 1);
}
proctype t2(byte id) priority 5 provided canRun(2) {
    call_fun(f2, 2);
}
