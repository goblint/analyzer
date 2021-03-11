// configuration defaults
#ifndef mutex_count
#define mutex_count 0
#endif

#ifndef cond_var_count
#define cond_var_count 0
#endif

// resources
mtype = {NONE, THREAD, MUTEX, COND_VAR}
typedef Resource {
  mtype type = NONE;
  byte id;
};

// threads
mtype = {NOTCREATED, READY, WAITING, DONE} // possible thread states

typedef Thread {
  mtype status = NOTCREATED;
  chan waitQueue = [thread_count] of { byte } // threads waiting on thread_join
  Resource waitingOnResource;
};
Thread threads[thread_count];

#if mutex_count
mtype = {UNLOCKED, LOCKED}
typedef Mutex {
  mtype status = UNLOCKED;
  byte tid;
  chan blockedQueue = [thread_count] of { byte };
};
Mutex mutexes[mutex_count];
#endif

#if cond_var_count
typedef CondVar {
  byte mid;
  chan waitQueue = [thread_count] of { byte };
};
CondVar cond_vars[cond_var_count];
#endif

// manage function calls at runtime to avoid inlining
// each proctype has its own: stack, sp
inline mark(pc) {
  sp++;
  stack[sp] = pc;
}

// helpers for scheduling etc.
#define notStarving(i)                                                                       \
  (always(threads[i].status == READY implies always eventually(threads[i].status == READY || \
                                                               threads[i].status == DONE)))

// LTL formulas
ltl not_starving  { allTasks(notStarving) }

inline preInit() {
  atomic {
    setReady(0); 
  }
}

#define canRun(thread_id)                                                      \
   (threads[thread_id].status == READY)

#define isWaiting(thread_id, resource_type, resource_id)                       \
  (threads[thread_id].status == WAITING &&                                     \
   threads[thread_id].waitingOnResource.type == resource_type &&               \
   threads[thread_id].waitingOnResource.id == resource_id)

#pragma mark - waiting/signaling

inline setWaiting(resource_type, resource_id) {
  printf("setWaiting: thread %d will wait for %e %d\n", tid, resource_type,
         resource_id);

  // thread should not be waiting for anything at this point
  assert(threads[tid].waitingOnResource.type == NONE);

  threads[tid].waitingOnResource.type = resource_type;
  threads[tid].waitingOnResource.id = resource_id;

  // update process threads[tid].status (provided clause will block immediately)
  threads[tid].status = WAITING;
}

inline setReady(thread_id) {
  printf("setReady: thread %d will be ready (was waiting for %e %d)\n",
         thread_id, threads[thread_id].waitingOnResource.type, threads[thread_id].waitingOnResource.id);

  threads[thread_id].waitingOnResource.type = NONE;
  threads[thread_id].status = READY;
}

#pragma mark - Thread logic

inline ThreadCreate(thread_id) {
  atomic {
    printf("ThreadCreate: id %d\n", thread_id);
    assert(threads[thread_id].status == NOTCREATED);

    setReady(thread_id);
  }
}

// pthread_join
inline ThreadWait(thread_id) {
  atomic {
    printf("ThreadWait: id %d\n", thread_id);
    assert(threads[thread_id].status != NOTCREATED);

    if
    :: threads[thread_id].status != DONE ->
      threads[thread_id].waitQueue!tid; // should block here
      setWaiting(THREAD, thread_id);
    :: threads[thread_id].status == DONE -> skip
    fi
  }
}

// called by the functions invoked by pthread_create
// notify the caller, that the thread is done computing
// similar to conditional var broadcast
inline ThreadBroadcast() {
  atomic {
    printf("ThreadBroadcast: id %d\n", tid);
    assert(threads[tid].waitingOnResource.type == NONE); // thread should not be waiting for anything at this point

    byte i;
    for (i in threads) {
      printf("ThreadBroadcast: check if thread %d is waiting. status[%d] = %e. waiting for %e %d\n",
              i, i, threads[i].status, threads[i].waitingOnResource.type, threads[i].waitingOnResource.id);
      if
      // thread is waiting for this thread
      :: isWaiting(i, THREAD, tid) ->
        printf("ThreadBroadcast: thread %d is waking up thread %d\n", tid, i);
        threads[tid].waitQueue?eval(i); // consume msg from queue
        setReady(i);
      :: else -> skip
      fi
    };

    threads[tid].status = DONE;
  }
}

#pragma mark - Mutex logic

inline MutexInit(mid) {
  atomic {
    printf("MutexInit: id %d\n", mid);
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
    :: mutexes[x].tid != tid -> skip
    :: mutexes[x].tid == tid ->
      printf("MutexUnlock unlocked: mutexes[%d] = UNLOCKED by %d\n", x, tid);
      mutexes[x].status = UNLOCKED;

      if
      // no processes waiting on this mutex -> skip
      :: empty(mutexes[x].blockedQueue) -> skip
      // wake up waiting process and reacquire the mutex
      :: nempty(mutexes[x].blockedQueue) -> 
        printf("MutexUnlock: %d threads in queue for mutex %d",
              len(mutexes[x].blockedQueue), x);
        byte i;

        for (i in threads) {
          printf("MutexUnlock: check if thread %d is waiting. status[%d] = %e. waiting for %e %d\n",
                i, i, threads[i].status, threads[i].waitingOnResource.type, threads[i].waitingOnResource.id);
          if
          // thread is waiting for this mutex and is at the front of its queue
          :: isWaiting(i, MUTEX, x) && mutexes[x].blockedQueue?[i] ->
            printf("MutexUnlock: thread %d is waking up thread %d\n", tid, i);
            mutexes[x].blockedQueue?eval(i); // consume msg from queue

            MutexLock(i, x);

            setReady(i);
            break
          :: else -> skip
          fi
        };
      fi;
    fi;
  }
}

#pragma mark - conditional vars

inline CondVarInit(cond_var_id) {
  atomic {
    printf("CondVarInit: id %d\n", cond_var_id);
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
               i, i, threads[i].status, threads[i].waitingOnResource.type, threads[i].waitingOnResource.id);
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
               i, i, threads[i].status, threads[i].waitingOnResource.type, threads[i].waitingOnResource.id);
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
