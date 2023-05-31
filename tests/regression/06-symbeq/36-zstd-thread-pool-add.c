// PARAM: --set ana.activated[+] symb_locks --set ana.activated[+] var_eq --set lib.activated[+] zstd --set exp.extraspecials[+] ZSTD_customMalloc --set exp.extraspecials[+] ZSTD_customCalloc
/* SPDX-License-Identifier: BSD-3-Clause */
/*
 * Copyright (c) Facebook, Inc.
 * All rights reserved.
 *
 * This code is a challenging example for race detection extracted from zstd.
 * Copyright (c) The Goblint Contributors
 */

#include<stdlib.h>
#include<pthread.h>
#include <goblint.h>
#define ZSTD_pthread_mutex_t            pthread_mutex_t
#define ZSTD_pthread_mutex_init(a, b)   pthread_mutex_init((a), (b))
#define ZSTD_pthread_mutex_destroy(a)   pthread_mutex_destroy((a))
#define ZSTD_pthread_mutex_lock(a)      pthread_mutex_lock((a))
#define ZSTD_pthread_mutex_unlock(a)    pthread_mutex_unlock((a))

#define ZSTD_pthread_cond_t             pthread_cond_t
#define ZSTD_pthread_cond_init(a, b)    pthread_cond_init((a), (b))
#define ZSTD_pthread_cond_destroy(a)    pthread_cond_destroy((a))
#define ZSTD_pthread_cond_wait(a, b)    pthread_cond_wait((a), (b))
#define ZSTD_pthread_cond_signal(a)     pthread_cond_signal((a))
#define ZSTD_pthread_cond_broadcast(a)  pthread_cond_broadcast((a))

#define ZSTD_pthread_t                  pthread_t
#define ZSTD_pthread_create(a, b, c, d) pthread_create((a), (b), (c), (d))
#define ZSTD_pthread_join(a, b)         pthread_join((a),(b))

#define ZSTD_malloc(s) malloc(s)
#define ZSTD_calloc(n,s) calloc((n), (s))
#define ZSTD_free(p) free((p))
#define ZSTD_memset(d,s,n) __builtin_memset((d),(s),(n))

typedef struct POOL_ctx_s POOL_ctx;

typedef void* (*ZSTD_allocFunction) (void* opaque, size_t size);
typedef void  (*ZSTD_freeFunction) (void* opaque, void* address);
typedef struct { ZSTD_allocFunction customAlloc; ZSTD_freeFunction customFree; void* opaque; } ZSTD_customMem;
typedef struct POOL_ctx_s ZSTD_threadPool;


void* ZSTD_customMalloc(size_t size, ZSTD_customMem customMem)
{
    if (customMem.customAlloc)
        return customMem.customAlloc(customMem.opaque, size);
    return ZSTD_malloc(size);
}

void* ZSTD_customCalloc(size_t size, ZSTD_customMem customMem)
{
    if (customMem.customAlloc) {
        /* calloc implemented as malloc+memset;
         * not as efficient as calloc, but next best guess for custom malloc */
        void* const ptr = customMem.customAlloc(customMem.opaque, size);
        ZSTD_memset(ptr, 0, size);
        return ptr;
    }
    return ZSTD_calloc(1, size);
}

void ZSTD_customFree(void* ptr, ZSTD_customMem customMem)
{
    if (ptr!=NULL) {
        if (customMem.customFree)
            customMem.customFree(customMem.opaque, ptr);
        else
            ZSTD_free(ptr);
    }
}



/*! POOL_create() :
 *  Create a thread pool with at most `numThreads` threads.
 * `numThreads` must be at least 1.
 *  The maximum number of queued jobs before blocking is `queueSize`.
 * @return : POOL_ctx pointer on success, else NULL.
*/
POOL_ctx* POOL_create(size_t numThreads, size_t queueSize);

POOL_ctx* POOL_create_advanced(size_t numThreads, size_t queueSize, ZSTD_customMem customMem);

/*! POOL_free() :
 *  Free a thread pool returned by POOL_create().
 */
void POOL_free(POOL_ctx* ctx);


/*! POOL_function :
 *  The function type that can be added to a thread pool.
 */
typedef void (*POOL_function)(void*);


static
#ifdef __GNUC__
__attribute__((__unused__))
#endif
ZSTD_customMem const ZSTD_defaultCMem = { NULL, NULL, NULL };  /**< this constant defers to stdlib's functions */


/* A job is a function and an opaque argument */
typedef struct POOL_job_s {
    POOL_function function;
    void *opaque;
} POOL_job;

struct POOL_ctx_s {
    ZSTD_customMem customMem;
    /* Keep track of the threads */
    ZSTD_pthread_t* threads;
    size_t threadCapacity;
    size_t threadLimit;

    /* The queue is a circular buffer */
    POOL_job *queue;
    size_t queueHead;
    size_t queueTail;
    size_t queueSize;

    /* The number of threads working on jobs */
    size_t numThreadsBusy;
    /* Indicates if the queue is empty */
    int queueEmpty;

    /* The mutex protects the queue */
    ZSTD_pthread_mutex_t queueMutex;
    /* Condition variable for pushers to wait on when the queue is full */
    ZSTD_pthread_cond_t queuePushCond;
    /* Condition variables for poppers to wait on when the queue is empty */
    ZSTD_pthread_cond_t queuePopCond;
    /* Indicates if the queue is shutting down */
    int shutdown;
};

/* POOL_thread() :
 * Work thread for the thread pool.
 * Waits for jobs and executes them.
 * @returns : NULL on failure else non-null.
 */
static void* POOL_thread(void* opaque) {
    POOL_ctx* const ctx = (POOL_ctx*)opaque;
    if (!ctx) { return NULL; }
    for (;;) {
        /* Lock the mutex and wait for a non-empty queue or until shutdown */
        ZSTD_pthread_mutex_lock(&ctx->queueMutex);

        while ( ctx->queueEmpty // RACE! (threadLimit)
            || (ctx->numThreadsBusy >= ctx->threadLimit) ) {
            if (ctx->shutdown) {
                /* even if !queueEmpty, (possible if numThreadsBusy >= threadLimit),
                 * a few threads will be shutdown while !queueEmpty,
                 * but enough threads will remain active to finish the queue */
                ZSTD_pthread_mutex_unlock(&ctx->queueMutex);
                return opaque;
            }
            ZSTD_pthread_cond_wait(&ctx->queuePopCond, &ctx->queueMutex);
        }
        /* Pop a job off the queue */
        {   POOL_job const job = ctx->queue[ctx->queueHead]; // TODO NORACE
            ctx->queueHead = (ctx->queueHead + 1) % ctx->queueSize; //NORACE
            ctx->numThreadsBusy++; //NORACE
            ctx->queueEmpty = (ctx->queueHead == ctx->queueTail); //NORACE
            /* Unlock the mutex, signal a pusher, and run the job */
            ZSTD_pthread_cond_signal(&ctx->queuePushCond);
            ZSTD_pthread_mutex_unlock(&ctx->queueMutex);

            job.function(job.opaque);

            /* If the intended queue size was 0, signal after finishing job */
            ZSTD_pthread_mutex_lock(&ctx->queueMutex);
            ctx->numThreadsBusy--; //NORACE
            ZSTD_pthread_cond_signal(&ctx->queuePushCond);
            ZSTD_pthread_mutex_unlock(&ctx->queueMutex);
        }
    }  /* for (;;) */
    __goblint_check(0);  //NOWARN (unreachable)
}

POOL_ctx* POOL_create(size_t numThreads, size_t queueSize) {
    return POOL_create_advanced(numThreads, queueSize, ZSTD_defaultCMem);
}

POOL_ctx* POOL_create_advanced(size_t numThreads, size_t queueSize,
                               ZSTD_customMem customMem)
{
    POOL_ctx* ctx;
    /* Check parameters */
    if (!numThreads) { return NULL; }
    /* Allocate the context and zero initialize */
    ctx = (POOL_ctx*)ZSTD_customCalloc(sizeof(POOL_ctx), customMem);
    if (!ctx) { return NULL; }
    /* Initialize the job queue.
     * It needs one extra space since one space is wasted to differentiate
     * empty and full queues.
     */
    ctx->queueSize = queueSize + 1;
    ctx->queue = (POOL_job*)ZSTD_customMalloc(ctx->queueSize * sizeof(POOL_job), customMem);
    ctx->queueHead = 0;
    ctx->queueTail = 0;
    ctx->numThreadsBusy = 0;
    ctx->queueEmpty = 1;
    {
        int error = 0;
        error |= ZSTD_pthread_mutex_init(&ctx->queueMutex, NULL);
        error |= ZSTD_pthread_cond_init(&ctx->queuePushCond, NULL);
        error |= ZSTD_pthread_cond_init(&ctx->queuePopCond, NULL);
        if (error) { POOL_free(ctx); return NULL; }
    }
    ctx->shutdown = 0;
    /* Allocate space for the thread handles */
    ctx->threads = (ZSTD_pthread_t*)ZSTD_customMalloc(numThreads * sizeof(ZSTD_pthread_t), customMem);
    ctx->threadCapacity = 0;
    ctx->customMem = customMem;
    /* Check for errors */
    if (!ctx->threads || !ctx->queue) { POOL_free(ctx); return NULL; }
    /* Initialize the threads */
    {   size_t i;
        for (i = 0; i < numThreads; ++i) {
            if (ZSTD_pthread_create(&ctx->threads[i], NULL, &POOL_thread, ctx)) {
                ctx->threadCapacity = i;
                POOL_free(ctx);
                return NULL;
        }   }
        ctx->threadCapacity = numThreads;
        ctx->threadLimit = numThreads; // RACE!
    }
    return ctx;
}

/*! POOL_join() :
    Shutdown the queue, wake any sleeping threads, and join all of the threads.
*/
static void POOL_join(POOL_ctx* ctx) {
    /* Shut down the queue */
    ZSTD_pthread_mutex_lock(&ctx->queueMutex);
    ctx->shutdown = 1; //NORACE
    ZSTD_pthread_mutex_unlock(&ctx->queueMutex);
    /* Wake up sleeping threads */
    ZSTD_pthread_cond_broadcast(&ctx->queuePushCond);
    ZSTD_pthread_cond_broadcast(&ctx->queuePopCond);
    /* Join all of the threads */
    {   size_t i;
        for (i = 0; i < ctx->threadCapacity; ++i) {
            ZSTD_pthread_join(ctx->threads[i], NULL);  /* note : could fail */
    }   }
}

void POOL_free(POOL_ctx *ctx) {
    if (!ctx) { return; }
    POOL_join(ctx);
    ZSTD_pthread_mutex_destroy(&ctx->queueMutex);
    ZSTD_pthread_cond_destroy(&ctx->queuePushCond);
    ZSTD_pthread_cond_destroy(&ctx->queuePopCond);
    ZSTD_customFree(ctx->queue, ctx->customMem);
    ZSTD_customFree(ctx->threads, ctx->customMem);
    ZSTD_customFree(ctx, ctx->customMem);
}

static int isQueueFull(POOL_ctx const* ctx) {
    if (ctx->queueSize > 1) {
        return ctx->queueHead == ((ctx->queueTail + 1) % ctx->queueSize);
    } else {
        return (ctx->numThreadsBusy == ctx->threadLimit) ||
               !ctx->queueEmpty;
    }
}


static void
POOL_add_internal(POOL_ctx* ctx, POOL_function function, void *opaque)
{
    POOL_job const job = {function, opaque};
    __goblint_check(ctx != NULL);
    if (ctx->shutdown) return;

    ctx->queueEmpty = 0;
    ctx->queue[ctx->queueTail] = job;
    ctx->queueTail = (ctx->queueTail + 1) % ctx->queueSize;
    ZSTD_pthread_cond_signal(&ctx->queuePopCond);
}

void POOL_add(POOL_ctx* ctx, POOL_function function, void* opaque)
{
    __goblint_check(ctx != NULL);
    ZSTD_pthread_mutex_lock(&ctx->queueMutex);
    /* Wait until there is space in the queue for the new job */
    while (isQueueFull(ctx) && (!ctx->shutdown)) {
        ZSTD_pthread_cond_wait(&ctx->queuePushCond, &ctx->queueMutex);
    }
    POOL_add_internal(ctx, function, opaque);
    ZSTD_pthread_mutex_unlock(&ctx->queueMutex);
}

void foo(void *arg) {
    __goblint_check(1); // reachable
}

int g;

void bar(void *arg) {
    g++; // RACE!
}

int main() {
    POOL_ctx* const ctx = POOL_create(20, 10);
    if (ctx) {
        POOL_add(ctx, foo, NULL);
        POOL_add(ctx, bar, NULL);
        POOL_add(ctx, bar, NULL);
    }
    return 0;
}
