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
  mtype state = NOTCREATED;
  chan waitQueue = [thread_count] of { byte } // threads waiting on thread_join
  Resource waitingOnResource;
};
Thread threads[thread_count];

#if mutex_count
mtype = {UNLOCKED, LOCKED}
typedef Mutex {
  mtype state = UNLOCKED;
  byte tid;
  chan blockedQueue = [thread_count] of { byte, byte };
};
Mutex mutexes[mutex_count];
#endif

#if cond_var_count
typedef CondVar {
  byte mid;
  chan waitQueue = [thread_count] of { byte, byte };
};
CondVar cond_vars[cond_var_count];
#endif

// manage function calls at runtime to avoid inlining
// each proctype has its own: stack, sp
inline mark(pc) {
  sp++;
  stack[sp] = pc;
}

// handle exit call in C programs
inline exit() {
  atomic {
    for (___i in threads) {
      threads[___i].state = DONE;
    }
  }
}

// helpers for scheduling etc.
#define notStarving(i)                                                                       \
  (always(threads[i].state == READY implies always eventually(threads[i].state == READY || \
                                                              threads[i].state == DONE)))

// LTL formulas
ltl not_starving  { allTasks(notStarving) }

#define canRun(thread_id) (threads[thread_id].state == READY)

#define isWaiting(thread_id, resource_type, resource_id)                      \
  (threads[thread_id].state == WAITING &&                                     \
   threads[thread_id].waitingOnResource.type == resource_type &&              \
   threads[thread_id].waitingOnResource.id == resource_id)

//pragma mark - waiting signaling

inline setWaiting(thread_id, resource_type, resource_id) {
  printf("setWaiting: thread %d will wait for %e %d\n", thread_id, resource_type,
         resource_id);

  // thread should not be waiting for anything at this point
  assert(threads[thread_id].waitingOnResource.type == NONE);

  threads[thread_id].waitingOnResource.type = resource_type;
  threads[thread_id].waitingOnResource.id = resource_id;

  // update process threads[tid].state (provided clause will block immediately)
  threads[thread_id].state = WAITING;
}

inline setReady(thread_id) {
  printf("setReady: thread %d will be ready (was waiting for %e %d)\n",
         thread_id, threads[thread_id].waitingOnResource.type, threads[thread_id].waitingOnResource.id);

  threads[thread_id].waitingOnResource.type = NONE;
  threads[thread_id].state = READY;
}

//pragma mark - Helper globals
byte ___i;
byte ___rand_num;

//pragma mark - Thread logic

inline ThreadCreate(thread_id) {
  atomic {
    printf("ThreadCreate: id %d\n", thread_id);

    setReady(thread_id);
  }
}

// pthread_join
inline ThreadWait(thread_id) {
  atomic {
    printf("ThreadWait: id %d\n", thread_id);

    if
    :: threads[thread_id].state != DONE ->
      threads[thread_id].waitQueue!tid; // should block here
      setWaiting(tid, THREAD, thread_id);
    :: threads[thread_id].state == DONE -> skip
    fi
  }
}

// called by the functions invoked by pthread_create
// notify the caller, that the thread is done computing
// similar to conditional var broadcast
inline ThreadExit() {
  atomic {
    printf("ThreadBroadcast: id %d\n", tid);
    assert(threads[tid].waitingOnResource.type == NONE); // thread should not be waiting for anything at this point

    do
    :: nempty(threads[tid].waitQueue) ->
        threads[tid].waitQueue?___i; // consume msg from queue

        assert(isWaiting(___i, THREAD, tid));
        printf("ThreadBroadcast: thread %d is waking up thread %d\n", tid, ___i);

        setReady(___i);

    :: empty(threads[tid].waitQueue) -> break
    od

    threads[tid].state = DONE;
  }
}

//pragma mark - Mutex logic

inline MutexInit(mid) {
  atomic {
    printf("MutexInit: id %d\n", mid);
  }
}

inline MutexLock(mid) {
  __MutexLock(tid, mid)
}

inline __MutexLock(thread_id, x) {
  atomic {
    if
    :: mutexes[x].state == LOCKED -> // TODO: reentrant mutex?
      printf("__MutexLock will block: mutexes[%d]\n", x);

      if
      :: full(mutexes[x].blockedQueue) -> // TODO can this happen?
        printf("FAIL: __MutexLock: queue is full\n");
        assert(false);
      :: nfull(mutexes[x].blockedQueue) ->
        printf("Thread %d put into queue for mutex %d\n", thread_id, x);

        select (___rand_num : 0..(thread_count - 1));

        // put random number and tid in queue in a sorted way
        // we do this in order to preserve POSIX semantics
        // of random mutex awakaning on mutex_unlock
        mutexes[x].blockedQueue!!___rand_num,thread_id;
      fi;

      setWaiting(thread_id, MUTEX, x); // blocks this process instantly
      // doc says: if stmt in atomic blocks, the rest will still remain atomic once it becomes executable.
      // atomicity is lost if one jumps out of the sequence (which might be the case with provided (...)).
      // revised: atomicity is broken if a statement inside the atomic blocks, but can continue as non-atomic
      // so, atomic is broken after setWaiting, but that's ok since we're done with WaitSemaphore anyway

    :: mutexes[x].state == UNLOCKED ->
      printf("__MutexLock locked: mutexes[%d] = LOCKED by %d\n", x, thread_id);
      mutexes[x].state = LOCKED;
      mutexes[x].tid = thread_id;
    fi
  }
}

inline MutexUnlock(x) {
  atomic {
    if
    :: mutexes[x].tid == tid ->
      printf("MutexUnlock unlocked: mutexes[%d] = UNLOCKED by %d\n", x, tid);
      mutexes[x].state = UNLOCKED;

      if
      // wake up waiting process and reacquire the mutex
      :: nempty(mutexes[x].blockedQueue) ->
        printf("MutexUnlock: %d threads in queue for mutex %d\n",
              len(mutexes[x].blockedQueue), x);

        mutexes[x].blockedQueue?_,___i;
        printf("MutexUnlock: thread %d is waking up thread %d\n", tid, ___i);

        __MutexLock(___i, x);
        setReady(___i);

      :: empty(mutexes[x].blockedQueue) -> skip
      fi;
    :: mutexes[x].tid != tid -> skip // undefined behavior
    fi;
  }
}

//pragma mark - conditional vars

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

    select (___rand_num : 0..(thread_count - 1));

    // put random number and tid in queue in a sorted way
    // we do this in order to preserve POSIX semantics
    // of random cond var awakaning on signaling

    cond_vars[cond_var_id].waitQueue!!___rand_num,tid

    // unlock(me);
    MutexUnlock(mut_id)
    setWaiting(tid, COND_VAR, cond_var_id)
  }
}

inline CondVarSignal(cond_var_id) {
  atomic {
    if
    // if there is a waiting process -> wake it up
    :: nempty(cond_vars[cond_var_id].waitQueue) ->
      printf("CondVarSignal: %d threads in queue for condition var %d\n",
             len(cond_vars[cond_var_id].waitQueue), cond_var_id);

      cond_vars[cond_var_id].waitQueue?_,___i;

      assert(isWaiting(___i, COND_VAR, cond_var_id));
      printf("CondVarSignal: thread %d is waking up thread %d\n", tid, ___i);

      setReady(___i);

      // reacquire the mutex lock
      __MutexLock(___i, cond_vars[cond_var_id].mid)
    :: empty(cond_vars[cond_var_id].waitQueue) -> skip
    fi;
  }
}

inline CondVarBroadcast(cond_var_id) {
  atomic {
    printf("CondVarSignal: %d threads in queue for condition var %d\n",
            len(cond_vars[cond_var_id].waitQueue), cond_var_id);

    do
      :: nempty(cond_vars[cond_var_id].waitQueue) ->
        // consume random msg from queue
        cond_vars[cond_var_id].waitQueue?_,___i;

        assert(isWaiting(___i, COND_VAR, cond_var_id));
        printf("CondVarSignal: thread %d is waking up thread %d\n", tid, ___i);

        setReady(___i);
        // reacquire the mutex lock
        __MutexLock(___i, cond_vars[cond_var_id].mid);

      :: empty(cond_vars[cond_var_id].waitQueue) -> break
    od

    // All waiting processes/threads are now waiting for the mutex to be released
  }
}
