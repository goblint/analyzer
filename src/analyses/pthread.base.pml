// configuration defaults
#ifndef PREEMPTION
#define PREEMPTION 1
#endif

#ifndef mutex_count
#define mutex_count 0
#endif

#ifndef cond_var_count
#define cond_var_count 0
#endif

// return codes
// #define SUCCESS 0   
// #define ERROR 1
mtype = { SUCCESS, ERROR }

// partition
mtype = { IDLE, COLD_START, WARM_START, NORMAL } // partition modes
mtype partitionMode = COLD_START;
// threads
mtype = { NOTCREATED, STOPPED, SUSPENDED, WAITING, READY, RUNNING, DONE } // possible thread states

// of all READY the scheduler will choose one and set it to RUNNING
// (with prios there will only be one choice, without it will choose non-determ.)
mtype status[thread_count] = NOTCREATED; // initialize all processes as not created

byte lockLevel; // scheduling only takes place if this is 0
byte exclusive; // id of process that has exclusive privilige to execute if lockLevel > 0
byte nperiodicWait; // number of processes that did a PeriodicWait and still wait
byte ncrit; // number of processes in critical section
byte threads_created;

// resources
mtype = { NONE, THREAD, MUTEX, COND_VAR, TIME }
typedef Wait { mtype resource; byte id; }
Wait waiting[thread_count];

// TODO: add/write a channel for thread signaling/waiting (pthread_join)
// TODO: add/write a channel for cond var signaling/waiting (pthread_join)

#if mutex_count
mtype = { FIFO, PRIO } // queuing discipline
byte mutexes[mutex_count];
byte mutexes_max[mutex_count];
chan mutexes_chan[mutex_count] = [thread_count] of { byte }
byte mutexes_created;
#endif

// manage function calls at runtime to avoid inlining
// each proctype has its own: stack, sp
inline mark(pc) {
    sp++;
    stack[sp] = pc;
}

// helpers for scheduling etc.
#define oneIs(v) checkStatus(==, v, ||)
#define oneIsNot(v) checkStatus(!=, v, ||)
#define allAre(v) checkStatus(==, v, &&)
#define noneAre(v) checkStatus(!=, v, &&)

// status: NOTCREATED, STOPPED, SUSPENDED, WAITING, READY, (RUNNING), DONE
// LTL formulas
#define notStarving(i) (always (status[i] == READY implies always eventually (status[i] == READY || status[i] == STOPPED || status[i] == DONE)))
ltl not_starving  { allTasks(notStarving) }
// ltl not_starving  { ! (eventually always oneIs(WAITING) || oneIs(SUSPENDED)) }
ltl not_waiting   { ! (eventually always oneIs(WAITING)) }
ltl not_suspended { ! (eventually always oneIs(SUSPENDED)) }
ltl all_created   { eventually always noneAre(NOTCREATED) }
// ltl created_then_ready { allAre(NOTCREATED U READY) }
// ltl all_ready { allAre(eventually READY) }
// periodic processes should never be done
// TODO: ltl periodic      { always periodic_noneAre(DONE) }
// TODO: ltl nonperiodic   { eventually nonperiodic_allAre(DONE) }
// starvation: process will always be READY but never RUNNING
// ltl pr { ! (eventually always oneIs(READY)) }

// TODO: pre-init, PREEMPTION?

// TODO: post init?

#define canRun(thread_id) ((status[thread_id] == READY || status[thread_id] == RUNNING) && (lockLevel == 0 || exclusive == thread_id) && (partitionMode == NORMAL || thread_id == 0))
#define isRunning(thread_id) (status[thread_id] == RUNNING)
#define isWaiting(thread_id, resource_type, resource_id) status[thread_id] == WAITING && waiting[thread_id].resource == resource_type && waiting[thread_id].id == resource_id

#pragma mark - waiting/signaling

inline setWaiting(resource_type, resource_id) {
    printf("setWaiting: thread %d will wait for %e %d\n", tid, resource_type, resource_id);
    waiting[tid].resource = resource_type; // waiting for...
    waiting[tid].id = resource_id;
    status[tid] = WAITING; // update process status (provided clause will block immediately)
}

inline setReady(thread_id) {
    printf("setReady: thread %d will be ready (was waiting for %e %d)\n", thread_id, waiting[thread_id].resource, waiting[thread_id].id);
    waiting[thread_id].resource = NONE;
    status[thread_id] = READY;
}

#pragma mark - Thread logic

inline ThreadCreate(thread_id) { 
	atomic {
		printf("ThreadCreate: id %d\n", thread_id);
		assert(status[thread_id] == NOTCREATED);
		status[thread_id] = STOPPED;
		waiting[thread_id].resource = NONE;
		threads_created++;
	}
}

// pthread_join
inline ThreadWait(thread_id) {
	atomic {
		printf("ThreadWait: id %d\n", thread_id);
		assert(status[thread_id] != NOTCREATED);
		assert(waiting[tid] == NONE); // thread should not be waiting for anything at this point
		setWaiting(THREAD, thread_id);
	}
}

// called by the functions invoked by pthread_create
// notify the caller, that the thread is done computing
// similar to conditional var broadcast
inline ThreadBroadcast() {
    atomic {
 		printf("ThreadSignal: id %d\n", tid);
		status[tid] = DONE;
		assert(waiting[tid] == NONE); // thread should not be waiting for anything at this point

        byte i;
        for (i in status) {
            printf("ThreadSignal: check if thread %d is waiting. status[%d] = %e. waiting for %e %d\n", i, i, status[i], waiting[i].resource, waiting[i].id);
            if
            // thread is waiting for this thread
            :: isWaiting(i, THREAD, tid) -> 
                printf("ThreadSignal: thread %d is waking up thread %d\n", tid, i);
                threads_chan[tid]?eval(i); // consume msg from queue
                setReady(i);
            :: !(isWaiting(i, THREAD, tid)  -> skip
            fi
        };
    }
}

#pragma mark - Mutex logic

// TODO: use ncrit for mutex and cond var;

inline MutexInit(mid) { 
	atomic {
		printf("MutexInit: id %d\n", mid);
		mutexes[mid] = 1;
		mutexes_max[mid] = 1;
		mutexes_created++;
	}
}

inline MutexWait(mid) {
    atomic {
        if
        :: mutexes[mid] == 0 ->
            printf("MutexWait will block: mutexes[%d] = %d\n", mid, mutexes[mid]);

            if
            :: full(mutexes_chan[mid]) -> // TODO can this happen?
                printf("FAIL: MutexWait: queue is full\n");
                assert(false);
            :: nfull(mutexes_chan[mid]) ->
                printf("Thread %d put into queue for mutex %d\n", id, mid);
                mutexes_chan[mid]!id; // put current process in queue
            fi;

            setWaiting(MUTEX, mid); // blocks this process instantly
            // doc says: if stmt in atomic blocks, the rest will still remain atomic once it becomes executable.
            // atomicity is lost if one jumps out of the sequence (which might be the case with provided (...)).
            // revised: atomicity is broken if a statement inside the atomic blocks, but can continue as non-atomic
            // so, atomic is broken after setWaiting, but that's ok since we're done with WaitSemaphore anyway
        :: mutexes[mid] > 0 ->
            printf("WaitSema will go through: mutexes[%d] = %d\n", mid, mutexes[mid]);
            mutexes[mid] = mutexes[mid] - 1;
        :: mutexes[mid] < 0 ->
            printf("FAIL: WaitSema: count<0: mutexes[%d] = %d\n", mid, mutexes[mid]);
            assert(false);
        fi
    }
}

inline MutexSignal(mid) {
    atomic {
        // filter processes waiting for mid
        if
        // no processes waiting on this semaphore -> increase count until max
        :: empty(mutexes_chan[mid]) ->
            printf("MutexSignal: empty queue\n");
            if
            :: mutexes[mid] < mutexes_max[mid] -> mutexes[mid] = mutexes[mid] + 1;
            :: !(mutexes[mid] < mutexes_max[mid]) -> skip
            fi
        // otherwise it stays the same, since we will wake up a waiting process
        :: nempty(mutexes_chan[mid]) -> // else doesn't work here because !empty is disallowed...
            printf("MutexSignal: %d threads in queue for mutex %d with count %d\n", len(mutexes_chan[mid]), mid, mutexes[mid]);
            byte i;

            // replace this loop by using mutexes_chan[mid]?i and waking up process i? what if it's not waiting?
            for (i in status) {
                printf("MutexSignal: check if thread %d is waiting. status[%d] = %e. waiting for %e %d\n", i, i, status[i], waiting[i].resource, waiting[i].id);
                if
                // thread is waiting for this semaphore and is at the front of its queue TODO prio queues
                :: isWaiting(i, MUTEX, mid) && mutexes_chan[mid]?[i] -> 
                    printf("MutexSignal: thread %d is waking up thread %d\n", tid, i);
                    mutexes_chan[mid]?eval(i); // consume msg from queue
                    setReady(i);
                    break
                :: !(isWaiting(i, MUTEX, mid) && mutexes_chan[mid]?[i]) -> skip
                fi
            };
        fi
    }
}

#pragma mark - conditional vars

inline CondVarInit(cond_var_id) {
    atomic {
        //TODO: add logic    
    }
}

inline CondVarWait(cond_var_id, mid) {
    atomic {
        //TODO: add logic    
    }
}

inline __CondVarSignal__(cond_var_id, signal_all) {
    atomic {
        //TODO: add logic
    }
}

inline CondVarSignal(cond_var_id) {
    __CondVarSignal__(cond_var_id, false)
}

inline CondVarBroadcast(cond_var_id) {
    __CondVarSignal__(cond_var_id, true)
}

#pragma mark - monitoring

// monitor for invariants
proctype monitor() {
    byte i;

    // at most 1 process may be in a critical region
    assert(ncrit == 0 || ncrit == 1);
    #if PREEMPTION
    assert(nperiodicWait <= nproc);
    #endif

    // #if nsema
    // // each semaphore value must be between 0 and max
    // for(i in mutexes) {
    //     assert(mutexes[i] >= 0 && mutexes[i] <= mutexes_max[i]);
    // }
    // #endif

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
