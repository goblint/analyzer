#include <stdlib.h>
#include <pthread.h>

struct ZSTD_CCtx_s {
  int bmi2;
};

typedef struct ZSTD_CCtx_s ZSTD_CCtx;

typedef struct {
  ZSTD_CCtx* cctx[1];
} ZSTDMT_CCtxPool;

void *t_fun(void *arg) {
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL); // enter multithreaded

  ZSTDMT_CCtxPool* const cctxPool = calloc(1, sizeof(ZSTDMT_CCtxPool));
  cctxPool->cctx[0] = malloc(sizeof(ZSTD_CCtx));
  if (!cctxPool->cctx[0]) // TODO NOWARN
    __goblint_check(1); // reachable
  else
    __goblint_check(1); // reachable
  return 0;
}
