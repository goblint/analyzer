// configuration defaults
#ifndef PREEMPTION
#define PREEMPTION 1
#endif
#ifndef nsema
#define nsema 0
#endif
#ifndef nevent
#define nevent 0
#endif

// arinc spin model
// constants
#define UP 1
#define DOWN 0

// partition
mtype = { IDLE, COLD_START, WARM_START, NORMAL } // partition modes
mtype partitionMode = COLD_START;
// processes
mtype = { NOTCREATED, STOPPED, SUSPENDED, WAITING, READY, RUNNING, DONE } // possible process states
// of all READY the scheduler will choose one and set it to RUNNING (with prios there will only be one choice, without it will choose non-determ.)
mtype status[nproc] = NOTCREATED; // initialize all processes as not created
byte lockLevel; // scheduling only takes place if this is 0
byte exclusive; // id of process that has exclusive privilige to execute if lockLevel > 0
byte nperiodicWait; // number of processes that did a PeriodicWait and still wait
byte ncrit; // number of processes in critical section
byte processes_created;
// resources
mtype = { NONE, SEMA, EVENT, TIME }
typedef Wait { mtype resource; byte id; }
Wait waiting[nproc];

#if nsema // semaphores
mtype = { FIFO, PRIO } // queuing discipline
byte semas[nsema];
byte semas_max[nsema];
chan semas_chan[nsema] = [nproc] of { byte }
byte semas_created;
#endif

#if nevent // events
bool events[nevent] = DOWN;
/* chan events_chan[nevent] = [nroc] of { byte } */
byte events_created;
#endif


// debug macros
// http://stackoverflow.com/questions/1644868/c-define-macro-for-debug-printing
/* #define pprintf(fmt, ...)   do { printf("Proc %d: " fmt, __VA_ARGS); } while(0) */
// #define pprintf(fmt, args...)   printf("Proc %d: ", id); printf(fmt, args)
byte tmp; // can't use skip as a placeholder. must do something. otherwise error "has unconditional self-loop"
#define todo   tmp=0

// helpers for scheduling etc.
#define oneIs(v) checkStatus(==, v, ||)
#define allAre(v) checkStatus(==, v, &&)
#define noneAre(v) checkStatus(!=, v, &&)
// inline preInit() {
//     status[0] = READY;
// }
#define preInit status[0] = RUNNING
inline postInit() {
    (partitionMode == NORMAL); // block spin init until arinc init sets mode
    // at this point every resource should have been created!
    // revised: this need not be the case on all paths!
    // NB: the extracted model is not precise enough with callstack_length = 0
    // TODO problem is, that then some paths in other processes later on are invalid if not all resources are created
    // e.g. P2 is not created but P1 calls Start(P2)
    // -> assert that process is created or do nothing
    assert(processes_created == nproc-1); // mainfun is not created
    #if nsema
    assert(semas_created == nsema);
    #endif
    #if nevent
    assert(events_created == nevent);
    #endif
    printf("Done with postInit!\n");
}
#define canRun(proc_id) ((status[proc_id] == READY || status[proc_id] == RUNNING) && (lockLevel == 0 || exclusive == proc_id) && (partitionMode == NORMAL || proc_id == 0))
#define isRunning(proc_id) (status[proc_id] == RUNNING)
inline setReady(proc_id) {
    printf("setReady: process %d will be ready (was waiting for %e %d)\n", proc_id, waiting[proc_id].resource, waiting[proc_id].id);
    waiting[proc_id].resource = NONE;
    status[proc_id] = READY;
}
inline setWaiting(resource_type, resource_id) {
    printf("setWaiting: process %d will wait for %e %d\n", id, resource_type, resource_id);
    waiting[id].resource = resource_type; // waiting for...
    waiting[id].id = resource_id;
    status[id] = WAITING; // update process status (provided clause will block immediately)
}
inline changeStatus(from, to) {
    byte i;
    for (i in status) {
        if
        :: status[i] == from -> status[i] = to
        :: else -> skip
        fi
    }
}
inline periodicWake() {
    byte i;
    for (i in status) {
        if
        :: status[i] == WAITING && waiting[i].resource == TIME -> status[i] = READY; waiting[i].resource = NONE; nperiodicWait--; break
        :: else -> skip
        fi
    }
}
// fallback to macro since inline doesn't support return values...
#define isWaiting(proc_id, resource_type, resource_id)    status[proc_id] == WAITING && waiting[proc_id].resource == resource_type && waiting[proc_id].id == resource_id
inline removeWaiting(proc_id) {
    #if nsema
    // remove process from waiting queues for all semas
    byte sema_id;
    for (sema_id in semas) {
        byte i;
        for (i : 1 .. len(semas_chan[sema_id])) {
            byte p;
            semas_chan[sema_id]?p;
            if
            :: p != proc_id -> semas_chan[sema_id]!p
            :: p == proc_id -> skip
            fi
        }
    }
    #endif
    waiting[proc_id].resource = NONE;
}


// ARINC functions collected by analysis
inline LockPreemption() { atomic {
    lockLevel++;
    exclusive = id;
} }
inline UnlockPreemption() { atomic {
    if
    :: lockLevel > 0 -> lockLevel--;
    :: lockLevel <= 0 -> skip
    fi
} }
inline SetPartitionMode(mode) { atomic {
    printf("SetPartitionMode(%e)\n", mode);
    partitionMode = mode;
} }
inline CreateProcess(proc_id, pri, per, cap) { atomic {
    printf("CreateProcess: id %d, priority %d, period %d, capacity %d\n", proc_id, pri, per, cap);
    assert(status[proc_id] == NOTCREATED);
    status[proc_id] = STOPPED;
    waiting[proc_id].resource = NONE;
    processes_created++;
} }
inline CreateErrorHandler(proc_id) { atomic {
    printf("CreateErrorHandler: id %d\n", proc_id);
    assert(status[proc_id] == NOTCREATED);
    status[proc_id] = STOPPED;
    waiting[proc_id].resource = NONE;
    processes_created++;
} }
inline Start(proc_id) { atomic {
    assert(status[proc_id] != NOTCREATED);
    removeWaiting(proc_id);
    status[proc_id] = READY;
    // TODO reset process if it is already running!
    // maybe insert after every statement: if restart[id] -> goto start_p1
} }
inline Stop(proc_id) { atomic {
    assert(status[proc_id] != NOTCREATED);
    removeWaiting(proc_id);
    status[proc_id] = STOPPED;
} }
inline Suspend(proc_id) { atomic {
    assert(status[proc_id] != NOTCREATED);
    status[proc_id] = SUSPENDED;
} }
inline Resume(proc_id) { atomic {
    assert(status[proc_id] != NOTCREATED);
    if
    :: status[proc_id] == SUSPENDED -> // only do something if process was really suspended
        if
        // if the process was waiting for something when it was suspended, change it back to waiting!
        :: waiting[proc_id].resource != NONE ->
            status[proc_id] = WAITING;
        :: ! (waiting[proc_id].resource != NONE) -> // otherwise resume
            status[proc_id] = READY;
        fi
    :: status[proc_id] != SUSPENDED -> skip
    fi
} }
inline CreateBlackboard(bb_id) { atomic {
    todo
} }
inline DisplayBlackboard(bb_id) { atomic {
    todo
} }
inline ReadBlackboard(bb_id) { atomic {
    todo
} }
inline ClearBlackboard(bb_id) { atomic {
    todo
} }
inline CreateSemaphore(sema_id, cur, max, queuing) { atomic {
    printf("CreateSemaphore: id %d, current %d, max %d, queuing %e\n", sema_id, cur, max, queuing);
    assert(queuing == FIFO); // TODO
    semas[sema_id] = cur;
    semas_max[sema_id] = max;
    semas_created++;
} }
// code uses 0/FIFO as queuing discipline
inline WaitSemaphore(sema_id) { atomic {
    // TODO priority queuing
    // FIFO queuing (channel needed for preserving order)
    if
    :: semas[sema_id] == 0 ->
        printf("WaitSema will block: semas[%d] = %d\n", sema_id, semas[sema_id]);
        if
        :: full(semas_chan[sema_id]) -> // TODO can this happen?
            printf("FAIL: WaitSema: queue is full\n");
            assert(false);
        :: nfull(semas_chan[sema_id]) ->
            printf("Process %d put into queue for sema %d\n", id, sema_id);
            semas_chan[sema_id]!id; // put current process in queue
        fi;
        setWaiting(SEMA, sema_id); // blocks this process instantly
        // doc says: if stmt in atomic blocks, the rest will still remain atomic once it becomes executable. atomicity is lost if one jumps out of the sequence (which might be the case with provided (...)).
        // revised: atomicity is broken if a statement inside the atomic blocks, but can continue as non-atomic
        // so, atomic is broken after setWaiting, but that's ok since we're done with WaitSemaphore anyway
    :: semas[sema_id] > 0 ->
        printf("WaitSema will go through: semas[%d] = %d\n", sema_id, semas[sema_id]);
        semas[sema_id] = semas[sema_id] - 1;
    :: semas[sema_id] < 0 ->
        printf("FAIL: WaitSema: count<0: semas[%d] = %d\n", sema_id, semas[sema_id]);
        assert(false);
    fi
} }
inline SignalSemaphore(sema_id) { atomic {
    // filter processes waiting for sema_id
    if
    // no processes waiting on this semaphore -> increase count until max
    :: empty(semas_chan[sema_id]) ->
        printf("SignalSema: empty queue\n");
        if
        :: semas[sema_id] < semas_max[sema_id] -> semas[sema_id] = semas[sema_id] + 1;
        :: !(semas[sema_id] < semas_max[sema_id]) -> skip
        fi
    // otherwise it stays the same, since we will wake up a waiting process
    :: nempty(semas_chan[sema_id]) -> // else doesn't work here because !empty is disallowed...
        printf("SignalSema: %d processes in queue for sema %d with count %d\n", len(semas_chan[sema_id]), sema_id, semas[sema_id]);
        byte i;
        for (i in status) {
            printf("SignalSema: check if process %d is waiting. status[%d] = %e. waiting for %e %d\n", i, i, status[i], waiting[i].resource, waiting[i].id);
            if
            :: isWaiting(i, SEMA, sema_id) && semas_chan[sema_id]?[i] -> // process is waiting for this semaphore and is at the front of its queue TODO prio queues
                printf("SignalSema: process %d is waking up process %d\n", id, i);
                semas_chan[sema_id]?eval(i); // consume msg from queue
                setReady(i);
                break
            :: !(isWaiting(i, SEMA, sema_id) && semas_chan[sema_id]?[i]) -> skip
            fi
        };
    fi
} }
inline CreateEvent(event_id) { atomic {
    todo;
    events_created++;
} }
inline WaitEvent(event_id) { atomic {
    if
    :: events[event_id] == DOWN -> setWaiting(EVENT, event_id);
    :: events[event_id] == UP -> skip // nothing to do
    fi
} }
inline SetEvent(event_id) { atomic {
    // filter processes waiting for event_id
    byte i;
    for (i in status) {
        if
        :: isWaiting(i, EVENT, event_id) ->
            setReady(i);
            // no break, since we want to wake all processes waiting for this event
        :: !(isWaiting(i, EVENT, event_id)) -> skip
        fi
    }
    events[event_id] = UP;
} }
inline ResetEvent(event_id) { atomic {
    events[event_id] = DOWN;
} }
inline TimedWait(time) { atomic {
    todo
} }
inline PeriodicWait() { atomic {
    // TODO PRIOS & PREEMPTION
    #if PREEMPTION
    status[id] = READY; // could be scheduled again right away
    #else
    status[id] = WAITING;
    waiting[id].resource = TIME;
    nperiodicWait++;
    #endif
} }


// verification helpers
inline WaitSignalSema(sema_id) {
    WaitSemaphore(sema_id);
    ncrit++;
    printf("Process %d is now in critical section!\n", id);
    //assert(ncrit == 1);	// critical section
    ncrit--;
    SignalSemaphore(sema_id);
}
// monitor for invariants
proctype monitor() {
    byte i;
    // at most 1 process may be in a critical region
    assert(ncrit == 0 || ncrit == 1);
    #if PREEMPTION
    assert(nperiodicWait <= nproc);
    #endif
    #if nsema
    // each semaphore value must be between 0 and max
    for(i in semas) {
        assert(semas[i] >= 0 && semas[i] <= semas_max[i]);
    }
    #endif
    // at every time at least one process should be READY or all should be DONE
    // atomic {
    //     byte nready = 0;
    //     byte ndone = 0;
    //     for(i in status) {
    //         if
    //         :: status[i] == READY -> nready++;
    //         :: status[i] == DONE -> ndone++;
    //         :: !(status[i] == READY || status[i] == DONE) -> skip
    //         fi
    //     }
    //     if
    //     :: nready == 0 && ndone < nproc -> printf("Deadlock detected (no process is READY (%d) but not all are DONE (%d))!\n", nready, ndone); assert(false);
    //     :: !(nready == 0 && ndone < nproc) -> skip
    //     fi
    // }
}
