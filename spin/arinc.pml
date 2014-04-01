// arinc spin model
// constants
#define UP 1
#define DOWN 0

// processes
mtype = { STOPPED, SUSPENDED, WAITING, READY, RUNNING } // possible states
mtype status[nproc] = READY; // initialize all processes as ready
byte ncrit; // number of processes in critical section
// resources
mtype = { NONE, SEMA, EVENT }
typedef Wait { mtype resource; byte id; }
Wait waiting[nproc];

#ifdef nsema // semaphores
byte semas[nsema];
byte semas_max[nsema];
#define init_sema(i, c, m)  semas[i] = c; semas_max[i] = m
chan semas_chan[nsema] = [nproc] of { byte }
#endif

#ifdef nevent // events
bool events[nevent] = DOWN;
/* chan events_chan[nevent] = [nroc] of { byte } */
#endif


// debug macros
// http://stackoverflow.com/questions/1644868/c-define-macro-for-debug-printing
/* #define pprintf(fmt, ...)   do { printf("Proc %d: " fmt, __VA_ARGS); } while(0) */
#define pprintf(fmt, args...)   printf("Proc %d: ", id); printf(fmt, args)

// helpers for scheduling etc.
inline setReady(proc_id) {
    printf("setReady: process %d will be ready (was waiting for %e %d)\n", proc_id, waiting[proc_id].resource, waiting[proc_id].id);
    waiting[proc_id].resource = NONE;
    waiting[proc_id].id = -1;
    status[proc_id] = READY;
}
inline setWaiting(resource_type, resource_id) {
    printf("setWaiting: process %d will wait for %e %d\n", id, resource_type, resource_id);
    waiting[id].resource = resource_type; // waiting for...
    waiting[id].id = resource_id;
    status[id] = WAITING; // update process status (provided clause will block immediately)
}
// fallback to macro since inline doesn't support return values...
#define isWaiting(proc_id, resource_type, resource_id)    status[proc_id] == WAITING && waiting[proc_id].resource == resource_type && waiting[proc_id].id == resource_id


// code uses 0/FIFO as queuing discipline
inline WaitSema(sema_id) { atomic {
    // TODO priority queuing
    // FIFO queuing (channel needed for preserving order)
    if
    :: semas[sema_id] == 0 ->
        printf("WaitSema will block: semas[%d] = %d\n", sema_id, semas[sema_id]);
        if
        :: full(semas_chan[sema_id]) -> // TODO can this happen?
            printf("FAIL: WaitSema: queue is full");
            assert(false);
        :: nfull(semas_chan[sema_id]) ->
            printf("Process %d put into queue for sema %d\n", id, sema_id);
            semas_chan[sema_id]!id; // put current process in queue
        fi;
        setWaiting(SEMA, sema_id); // blocks this process instantly
        // jspin's guided mode runs into assertion violations because atomicity is lost here. verify yields no errors though...
        // doc says: if stmt in atomic blocks, the rest will still remain atomic once it becomes executable. atomicity is lost if one jumps out of the sequence (which might be the case with provided (...)).
    :: semas[sema_id] > 0 ->
        printf("WaitSema will go through: semas[%d] = %d\n", sema_id, semas[sema_id]);
        semas[sema_id] = semas[sema_id] - 1;
    :: else ->
        printf("FAIL: WaitSema: count<0: semas[%d] = %d\n", sema_id, semas[sema_id]);
        assert(false);
    fi
} }
inline SignalSema(sema_id) { atomic {
    // filter processes waiting for sema_id
    if
    // no processes waiting on this semaphore -> increase count until max
    :: empty(semas_chan[sema_id]) ->
        printf("SignalSema: empty queue\n");
        if
        :: semas[sema_id] < semas_max[sema_id] -> semas[sema_id] = semas[sema_id] + 1;
        :: else -> skip
        fi
    // otherwise it stays the same, since we will wake up a waiting process
    :: nempty(semas_chan[sema_id]) -> // else doesn't work here because !empty is disallowed...
        printf("SignalSema: %d processes in queue\n", len(semas_chan[sema_id]));
        byte i;
        byte c;
        for (i in status) {
            printf("SignalSema: check if process %d is waiting...\n", i);
            if
            :: i!=id && isWaiting(i, SEMA, sema_id) && semas_chan[sema_id]?[i] -> // process is waiting for this semaphore and is at the front of its queue TODO prio queues
                printf("SignalSema: process %d is waking up process %d\n", id, i);
                c++;
                semas_chan[sema_id]?eval(i); // consume msg from queue
                setReady(i);
                break
            :: else -> skip
            fi
        };
        assert(c==0 || c==1);
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
        :: else -> skip
        fi
    }
    events[event_id] = UP;
} }
inline ResetEvent(event_id) { atomic {
    events[event_id] = DOWN;
} }
inline WaitEvent(event_id) { atomic {
    if
    :: events[event_id] == DOWN -> setWaiting(EVENT, event_id);
    :: events[event_id] == UP -> skip; // nothing to do
    fi
} }
inline Suspend(id) { atomic {
    status[id] = SUSPENDED;
} }
inline Resume(id) { atomic {
    // TODO e.g. p1 was waiting and gets suspended -> should change to waiting again!
    status[id] == SUSPENDED -> status[id] = READY;
} }
inline PeriodicWait() { atomic {
    skip
} }
inline DisplayBlackboard() { atomic {
    skip
} }
inline ReadBlackboard() { atomic {
    skip
} }


// verification helpers
inline WaitSignalSema(sema_id) {
    WaitSema(sema_id);
    ncrit++;
    printf("Process %d is now in critical section!\n", id);
    //assert(ncrit == 1);	// critical section
    ncrit--;
    SignalSema(sema_id);
}
// monitor for invariants
proctype monitor() {
    byte i;
    // at most 1 process may be in a critical region
    assert(ncrit == 0 || ncrit == 1);
    // each semaphore value must be between 0 and max
    for(i in semas) {
        assert(semas[i] >= 0 && semas[i] <= semas_max[i]);
    }
    // at least one process should be READY
    byte nready = 0;
    for(i in status) {
        if
        :: status[i] == READY -> nready++;
        :: else -> skip
        fi
    }
    if
    :: nready == 0 -> printf("Deadlock detected (no process is READY)!\n"); assert(false);
    :: else -> skip
    fi
}

