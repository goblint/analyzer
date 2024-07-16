// SKIP PARAM: --set ana.activated[+] region
// FIXPOINT
#include<stdlib.h>
#include<pthread.h>
#include<goblint.h>

typedef struct POOL_job_s {
    void *opaque;
} POOL_job;

typedef struct POOL_ctx_s {
    POOL_job *queue;
} POOL_ctx;

POOL_ctx* ctx_global;

POOL_ctx* POOL_create(size_t numThreads, size_t queueSize)
{
    POOL_ctx* ctx_create;
    ctx_create = (POOL_ctx*)malloc(sizeof(POOL_ctx));
    ctx_create->queue = (POOL_job*)malloc(queueSize * sizeof(POOL_job));

    int r; // rand
    if (r)
        ctx_global = ctx_create; // pretend escape
    return ctx_create;
}

int main() {
    while (1) {
        POOL_ctx *ctx_main;
        ctx_main = POOL_create(20, 10);
    }
}
