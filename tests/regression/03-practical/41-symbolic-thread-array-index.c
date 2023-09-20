// Per-thread array index passed via argument.
#include <stdlib.h>
#include <pthread.h>
#include <goblint.h>
extern int __VERIFIER_nondet_int();

int *datas;

void *thread(void *arg) {
  int i = arg;
  datas[i] = __VERIFIER_nondet_int(); // NORACE
  return NULL;
}

int main() {
  int threads_total = __VERIFIER_nondet_int();
  __goblint_assume(threads_total >= 0);

  pthread_t *tids = malloc(threads_total * sizeof(pthread_t));
  datas = malloc(threads_total * sizeof(int));

  // create threads
  for (int i = 0; i < threads_total; i++) {
    pthread_create(&tids[i], NULL, &thread, i); // may fail but doesn't matter
  }

  // join threads
  for (int i = 0; i < threads_total; i++) {
    pthread_join(tids[i], NULL);
  }

  free(tids);
  free(datas);

  return 0;
}
