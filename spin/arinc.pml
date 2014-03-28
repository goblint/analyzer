// arinc spin model
// constants
#define UP 1
#define DOWN 0

// processes
mtype = { STOPPED, SUSPENDED, WAITING, READY, RUNNING } // possible states
mtype status[nproc] = READY; // initialize all processes as ready
// resources
mtype = { NONE, SEMA, EVENT }
typedef Wait { mtype resource; byte id; }
Wait waiting[nproc];
// semaphores
byte semas[nsema];
byte semas_max[nsema];
chan semas_chan[nsema] = [nproc] of { byte }
byte ncrit;
// events
bool events[nevent] = DOWN;
/* chan events_chan[nevent] = [nroc] of { byte } */


// helpers for scheduling etc.
inline setReady(proc_id) {
    status[proc_id] = READY;
    waiting[proc_id].resource = NONE;
    waiting[proc_id].id = -1;
}
inline setWaiting(resource_type, resource_id) {
    status[id] = WAITING; // update process status
    waiting[id].resource = resource_type; // waiting for...
    waiting[id].id = resource_id;
}
/* #define isWaiting(resource, resource_id) status[id] == WAITING && waiting[id].resource == resource && waiting[id].id == resource_id */
inline isWaiting(resource, resource_id) {
    // eval doesn't work :(
    if
    :: (status[id] == WAITING && waiting[id].resource == resource && waiting[id].id == resource_id) -> true
    :: else -> false
    fi
}


// code uses 0/FIFO as queuing discipline
inline WaitSema(sema_id) { atomic {
    // TODO priority queuing
    // FIFO queuing (channel needed for preserving order)
    if
    :: semas[sema_id] == 0 ->
        printf("WaitSema will block: semas[%d] = %d\n", sema_id, semas[sema_id]);
        setWaiting(SEMA, sema_id); // will block process after this atomic
        semas_chan[sema_id]!id; // put current process in queue TODO can this be full and block?
    :: semas[sema_id] > 0 ->
        /* semas_chan?[sema_id]; // query: is there an element id at the front? */
        /* byte tmp; */
        /* semas_chan[sema_id]?tmp; */
        /* assert(tmp == id); */
        /* semas_chan[sema_id]?eval(id); // turn id into a constant and receive msg once at the front. blocks if another id is at the front (also breaks atomic chain). */
        printf("WaitSema: semas[%d] = %d\n", sema_id, semas[sema_id]);
        semas[sema_id] = semas[sema_id] - 1;
    fi
} }
inline SignalSema(sema_id) { atomic {
    // filter processes waiting for sema_id
    if
    // no processes waiting on this semaphore -> increase count
    :: empty(semas_chan[sema_id]) -> semas[sema_id] = semas[sema_id] + 1;
    // otherwise it stays the same, since we will wake up a waiting process
    :: nempty(semas_chan[sema_id]) -> // else doesn't work here because !empty is disallowed...
        int i;
        for (i in status) {
            if
            /* :: isWaiting(SEMA, sema_id) && semas_chan[sema_id]?[i] -> */
            :: status[id] == WAITING && waiting[id].resource == SEMA && waiting[id].id == sema_id && semas_chan[sema_id]?[i] -> // process is waiting for this semaphore and is at the front of its queue TODO prio queues
                semas_chan[sema_id]?eval(i); // consume msg from queue
                setReady(i);
                break
            :: else -> skip
            fi
        }
    fi
} }
inline SetEvent(event_id) { atomic {
    // filter processes waiting for event_id
    int i;
    for (i in status) {
        if
        /* :: isWaiting(EVENT, event_id) -> */
        :: status[id] == WAITING && waiting[id].resource == EVENT && waiting[id].id == event_id ->
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
    mtype event = events[event_id];
    if
    :: event == DOWN -> setWaiting(EVENT, event_id);
    :: event == UP -> skip; // nothing to do
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
inline WaitSignalSema(sema_id) { // TODO support FIFO and prio queuing
    WaitSema(sema_id);
    ncrit++;
    //assert(ncrit == 1);	// critical section
    ncrit--;
    SignalSema(sema_id);
}
// monitor for sanity checks
active proctype monitor() {
    // at most 1 process may be in a critical region
    assert(ncrit == 0 || ncrit == 1);
}

