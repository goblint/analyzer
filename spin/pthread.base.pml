// configuration defaults
#ifndef mutex_count
#define mutex_count 0
#endif

#ifndef cond_var_count
#define cond_var_count 0
#endif

// return codes
mtype = {SUCCESS, ERROR}

// partition
mtype = {IDLE, COLD_START, WARM_START, NORMAL} // partition modes

// threads
mtype = {NOTCREATED, STOPPED, SUSPENDED, WAITING,
         READY,      RUNNING, DONE} // possible thread states

// of all READY the scheduler will choose one and set it to RUNNING
// (with prios there will only be one choice, without it will choose
// non-determ.)
mtype status[thread_count] = NOTCREATED; // initialize all processes as not created

byte ncrit; // number of processes in critical section
byte threads_created;
//TODO: [0] or [thread_count]
chan threads_chan[thread_count] = [thread_count] of { byte }

// resources
mtype = {NONE, THREAD, MUTEX, COND_VAR, TIME}
typedef Wait {
  mtype resource = NONE;
  byte id;
}
Wait waiting[thread_count];

#if mutex_count
mtype = {UNLOCKED, LOCKED}
typedef Mutex {
  mtype status = UNLOCKED;
  byte tid;
  chan blockedQueue = [thread_count] of { byte };
};

Mutex mutexes[mutex_count];
byte mutexes_created;
#endif

#if cond_var_count
typedef CondVar {
  byte mid;
  chan waitQueue = [thread_count] of { byte };
}

byte cond_vars[cond_var_count];
byte cond_vars_created;
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
#define notStarving(i)                                                         \
  (always(status[i] == READY implies always eventually(status[i] == READY ||   \
                                                       status[i] == STOPPED || \
                                                       status[i] == DONE)))

// LTL formulas
ltl not_waiting   { ! (eventually always oneIs(WAITING)) }
ltl not_starving  { allTasks(notStarving) }
ltl not_suspended { ! (eventually always oneIs(SUSPENDED)) }
ltl all_created   { eventually always noneAre(NOTCREATED) }

inline preInit() {
  status[0] = RUNNING;
}

inline postInit() {
  // assert(threads_created == thread_count-1); // mainfun is not created

  /* #if mutex_count */
  /* assert(mutexes_created == mutex_count); */
  /* #endif */

  /* #if cond_var_count */
  /* assert(cond_vars_created == cond_var_count); */
  /* #endif */
}

#define canRun(thread_id)                                                      \
   (status[thread_id] == READY || status[thread_id] == RUNNING)
#define isRunning(thread_id) (status[thread_id] == RUNNING)
#define isWaiting(thread_id, resource_type, resource_id)                       \
  (status[thread_id] == WAITING &&                                             \
   waiting[thread_id].resource == resource_type &&                             \
   waiting[thread_id].id == resource_id)

#pragma mark - waiting/signaling

inline setWaiting(resource_type, resource_id) {
  printf("setWaiting: thread %d will wait for %e %d\n", tid, resource_type,
         resource_id);

  // thread should not be waiting for anything at this point
  assert(waiting[tid].resource == NONE);

  waiting[tid].resource = resource_type;
  waiting[tid].id = resource_id;

  // update process status (provided clause will block immediately)
  status[tid] = WAITING;
}

inline setReady(thread_id) {
  printf("setReady: thread %d will be ready (was waiting for %e %d)\n",
         thread_id, waiting[thread_id].resource, waiting[thread_id].id);

  waiting[thread_id].resource = NONE;
  status[thread_id] = READY;
}

#pragma mark - Thread logic

inline ThreadCreate(thread_id) {
  atomic {
    printf("ThreadCreate: id %d\n", thread_id);
    assert(status[thread_id] == NOTCREATED);

    status[thread_id] = RUNNING;
    threads_created++;
  }
}

// pthread_join
inline ThreadWait(thread_id) {
  atomic {
    printf("ThreadWait: id %d\n", thread_id);
    assert(status[thread_id] != NOTCREATED);

    if
    ::  status[thread_id] != DONE ->
          threads_chan[thread_id]!tid; // should block here
          setWaiting(THREAD, thread_id);
    ::  status[thread_id] == DONE -> skip
    fi
  }
}

// called by the functions invoked by pthread_create
// notify the caller, that the thread is done computing
// similar to conditional var broadcast
inline ThreadBroadcast() {
  atomic {
    printf("ThreadBroadcast: id %d\n", tid);
    assert(waiting[tid].resource == NONE); // thread should not be waiting for anything at this point

    byte i;
    for (i in status) {
      printf("ThreadBroadcast: check if thread %d is waiting. status[%d] = %e. waiting for %e %d\n",
              i, i, status[i], waiting[i].resource, waiting[i].id);
      if
      // thread is waiting for this thread
      :: isWaiting(i, THREAD, tid) ->
        printf("ThreadBroadcast: thread %d is waking up thread %d\n", tid, i);
        threads_chan[tid]?eval(i); // consume msg from queue
        setReady(i);
      :: else -> skip
      fi
    };

	  status[tid] = DONE;
  }
}

#pragma mark - Mutex logic

inline MutexInit(mid) {
  atomic {
    printf("MutexInit: id %d\n", mid);
    mutexes_created++;
  }
}

inline MutexLock(mid) {
  atomic {
    if
    :: mutexes[mid].status == LOCKED -> // TODO: reentrant mutex?
      printf("MutexLock will block: mutexes[%d]", mid);

      if
      :: full(mutexes[mid].blockedQueue) -> // TODO can this happen?
        printf("FAIL: MutexLock: queue is full\n");
        assert(false);
      :: nfull(mutexes[mid].blockedQueue) ->
        printf("Thread %d put into queue for mutex %d\n", tid, mid);
        mutexes[mid].blockedQueue!tid; // put current process in queue
      fi;

      setWaiting(MUTEX, mid); // blocks this process instantly
      // doc says: if stmt in atomic blocks, the rest will still remain atomic once it becomes executable.
      // atomicity is lost if one jumps out of the sequence (which might be the case with provided (...)).
      // revised: atomicity is broken if a statement inside the atomic blocks, but can continue as non-atomic
      // so, atomic is broken after setWaiting, but that's ok since we're done with WaitSemaphore anyway

    :: mutexes[mid].status == UNLOCKED ->
      printf("MutexLock locked: mutexes[%d] = LOCKED by %d\n", mid, tid);
      mutexes[mid].status = LOCKED;
      mutexes[mid].tid = tid;
    fi
  }
}

inline MutexUnlock(mid) {
  atomic {
    if
    // no processes waiting on this mutex -> skip
    :: empty(mutexes[mid].blockedQueue) || mutexes[mid].tid != tid -> skip
    // otherwise it stays the same, since we will wake up a waiting process
    :: nempty(mutexes[mid].blockedQueue) && mutexes[mid].tid == tid ->
      printf("MutexUnlock: %d threads in queue for mutex %d",
             len(mutexes[mid].blockedQueue), mid);
      byte i;

      for (i in status) {
        printf("MutexUnlock: check if thread %d is waiting. status[%d] = %e. waiting for %e %d\n",
               i, i, status[i], waiting[i].resource, waiting[i].id);
        if
        // thread is waiting for this mutex and is at the front of its queue
        :: isWaiting(i, MUTEX, mid) && mutexes[mid].blockedQueue?[i] ->
          printf("MutexUnlock: thread %d is waking up thread %d\n", tid, i);
          mutexes[mid].blockedQueue?eval(i); // consume msg from queue
          setReady(i);
          break
        :: else -> skip
        fi
      };
    fi;

    printf("MutexUnlock unlocked: mutexes[%d] = UNLOCKED by %d\n", mid, tid);
    mutexes[mid].status = UNLOCKED;
  }
}

#pragma mark - conditional vars

inline CondVarInit(cond_var_id) {
  atomic {
    printf("CondVarInit: id %d\n", cond_var_id);
    cond_vars_created++;
  }
}

// void wait(CondVar *cv, Mutex *me) {
//     if (me->tid != tid) failure("illegal wait");
//     enqueue(cv->wq, tid);
//
//     unlock(me);
//     next();
//     lock(me);
// }

inline CondVarWait(cond_var_id, mid) {
  atomic {
    // if (me->tid != tid) failure("illegal wait");
    assert(mutexes[mid].tid == tid)

    cond_vars[cond_var_id].mid = mid

    // enqueue(cv->wq, tid);
    cond_vars[cond_var_id].waitQueue!tid

    // unlock(me);
    MutexUnlock(mid)
    SetWaiting(COND_VAR, cond_var_id)
  }
}

// void signal(CondVar *cv) {
//     tid wait_tid = dequeue(cv->wq);
//     if (wait_tid >= 0) enqueue(RQ, wait_tid)
// }

inline CondVarSignal(cond_var_id) {
  atomic {
    if
    // no processes waiting on this condition var -> skip
    :: empty(cond_vars[cond_var_id].waitQueue) -> skip
    // otherwise it stays the same, since we will wake up a waiting process
    :: nempty(mutexes[mid].waitQueue) ->
      printf("CondVarSignal: %d threads in queue for condition var %d",
             len(cond_vars[cond_var_id].waitQueue), cond_var_id);
      byte i;

      for (i in status) {
        printf("CondVarSignal: check if thread %d is waiting. status[%d] = %e. waiting for %e %d\n",
               i, i, status[i], waiting[i].resource, waiting[i].id);
        if
        // thread is waiting for this condition var and is at the front of its queue
        :: isWaiting(i, COND_VAR, cond_var_id) && cond_vars[cond_var_id].waitQueue?[i] ->
          printf("CondVarSignal: thread %d is waking up thread %d\n", tid, i);

          // consume msg from queue
          cond_vars[cond_var_id].waitQueue?eval(i);

          // reacquire the mutex lock
          assert(mutexes[cond_vars[cond_var_id].mid].status == UNLOCKED);
          MutexLock(cond_vars[cond_var_id].mid);

          setReady(i);
          break
        :: else -> skip
        fi
      };
    fi;
  }
}

inline CondVarBroadcast(cond_var_id) {
  atomic {
    // TODO: is this legit?
    CondVarSignal(cond_var_id);
  }
}

#pragma mark - monitoring

// monitor for invariants
proctype monitor() {
  byte i;

  // at most 1 process may be in a critical region
  assert(ncrit == 0 || ncrit == 1);

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
  //     :: nready == 0 && ndone < nproc -> printf("Deadlock detected (no
  //     process is READY (%d) but not all are DONE (%d))!\n", nready, ndone);
  //     assert(false);
  //     :: !(nready == 0 && ndone < nproc) -> skip
  //     fi
  // }
}
