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

CondVar cond_vars[cond_var_count];
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

inline MutexLock(thread_id, x) {
  atomic {
    if
    :: mutexes[x].status == LOCKED -> // TODO: reentrant mutex?
      printf("MutexLock will block: mutexes[%d]", x);

      if
      :: full(mutexes[x].blockedQueue) -> // TODO can this happen?
        printf("FAIL: MutexLock: queue is full\n");
        assert(false);
      :: nfull(mutexes[x].blockedQueue) ->
        printf("Thread %d put into queue for mutex %d\n", thread_id, x);
        mutexes[x].blockedQueue!thread_id; // put current process in queue
      fi;

      setWaiting(MUTEX, x); // blocks this process instantly
      // doc says: if stmt in atomic blocks, the rest will still remain atomic once it becomes executable.
      // atomicity is lost if one jumps out of the sequence (which might be the case with provided (...)).
      // revised: atomicity is broken if a statement inside the atomic blocks, but can continue as non-atomic
      // so, atomic is broken after setWaiting, but that's ok since we're done with WaitSemaphore anyway

    :: mutexes[x].status == UNLOCKED ->
      printf("MutexLock locked: mutexes[%d] = LOCKED by %d\n", x, thread_id);
      mutexes[x].status = LOCKED;
      mutexes[x].tid = thread_id;
    fi
  }
}

inline MutexUnlock(x) {
  atomic {
    if
    // no processes waiting on this mutex -> skip
    :: empty(mutexes[x].blockedQueue) || mutexes[x].tid != tid -> skip
    // otherwise it stays the same, since we will wake up a waiting process
    :: nempty(mutexes[x].blockedQueue) && mutexes[x].tid == tid ->
      printf("MutexUnlock: %d threads in queue for mutex %d",
             len(mutexes[x].blockedQueue), x);
      byte i;

      for (i in status) {
        printf("MutexUnlock: check if thread %d is waiting. status[%d] = %e. waiting for %e %d\n",
               i, i, status[i], waiting[i].resource, waiting[i].id);
        if
        // thread is waiting for this mutex and is at the front of its queue
        :: isWaiting(i, MUTEX, x) && mutexes[x].blockedQueue?[i] ->
          printf("MutexUnlock: thread %d is waking up thread %d\n", tid, i);
          mutexes[x].blockedQueue?eval(i); // consume msg from queue
          setReady(i);
          break
        :: else -> skip
        fi
      };
    fi;

    printf("MutexUnlock unlocked: mutexes[%d] = UNLOCKED by %d\n", x, tid);
    mutexes[x].status = UNLOCKED;
  }
}

#pragma mark - conditional vars

inline CondVarInit(cond_var_id) {
  atomic {
    printf("CondVarInit: id %d\n", cond_var_id);
    cond_vars_created++;
  }
}

inline CondVarWait(cond_var_id, mut_id) {
  atomic {
    printf("CondVarWait: id %d\n", cond_var_id);

    // if (me->tid != tid) failure("illegal wait");
    assert(mutexes[mut_id].tid == tid)

    cond_vars[cond_var_id].mid = mut_id

    // enqueue(cv->wq, tid);
    cond_vars[cond_var_id].waitQueue!tid

    // unlock(me);
    MutexUnlock(mut_id)
    setWaiting(COND_VAR, cond_var_id)
  }
}

inline CondVarSignal(cond_var_id) {
  atomic {
    if
    // no processes waiting on this condition var -> skip
    :: empty(cond_vars[cond_var_id].waitQueue) -> skip
    // otherwise it stays the same, since we will wake up a waiting process
    :: nempty(cond_vars[cond_var_id].waitQueue) ->
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

          setReady(i);
          // reacquire the mutex lock
          MutexLock(i, cond_vars[cond_var_id].mid);

          break
        :: else -> skip
        fi
      };
    fi;
  }
}

inline CondVarBroadcast(cond_var_id) {
  atomic {
    if
    // no processes waiting on this condition var -> skip
    :: empty(cond_vars[cond_var_id].waitQueue) -> skip
    // otherwise it stays the same, since we will wake up a waiting process
    :: nempty(cond_vars[cond_var_id].waitQueue) ->
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

          setReady(i);
          // reacquire the mutex lock
          // All waititng processes/threads are now waiting for the mutex to be released
          MutexLock(cond_vars[cond_var_id].mut_id);
        :: else -> skip
        fi
      };
    fi;
  }
}
