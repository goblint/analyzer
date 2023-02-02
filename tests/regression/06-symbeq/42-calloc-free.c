// PARAM: --set ana.activated[+] symb_locks
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

typedef struct POOL_ctx_s POOL_ctx;

struct POOL_ctx_s {
    pthread_t* threads;
    size_t numThreadsBusy;
    pthread_mutex_t queueMutex;
};

static void* POOL_thread(void* opaque) {
    POOL_ctx* const ctx = (POOL_ctx*)opaque;
    for (;;) {
        /* Lock the mutex and wait for a non-empty queue or until shutdown */
        pthread_mutex_lock(&ctx->queueMutex);
        ctx->numThreadsBusy++; // RACE!
        pthread_mutex_unlock(&ctx->queueMutex);
    }
}

void POOL_free(POOL_ctx *ctx) {
    pthread_mutex_destroy(&ctx->queueMutex);
    free(ctx->threads);
    free(ctx); // RACE!
}

POOL_ctx* POOL_create(size_t numThreads) {
    POOL_ctx* ctx;
    ctx = (POOL_ctx*)calloc(1, sizeof(POOL_ctx));
    if (!ctx) { return NULL; }
    ctx->numThreadsBusy = 0;
    {
        int error = 0;
        error |= pthread_mutex_init(&ctx->queueMutex, NULL);
        if (error) { POOL_free(ctx); return NULL; }
    }
    ctx->threads = (pthread_t*)malloc(numThreads * sizeof(pthread_t));
    if (!ctx->threads) { POOL_free(ctx); return NULL; }
    {   size_t i;
        for (i = 0; i < numThreads; ++i) {
            if (pthread_create(&ctx->threads[i], NULL, &POOL_thread, ctx)) {
                POOL_free(ctx);
                return NULL;
        }   }
    }
    return ctx;
}


int main() {
    POOL_ctx* const ctx = POOL_create(20);
}
