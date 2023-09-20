// Per-thread array pointers passed via argument but initialized before thread create.
// Extracted from silver searcher.
#include <stdlib.h>
#include <pthread.h>
#include <goblint.h>
extern int __VERIFIER_nondet_int();

void *thread(void *arg) {
  int *p = arg;
  int i = *p; // NORACE
  return NULL;
}

int main() {
  int threads_total = __VERIFIER_nondet_int();
  __goblint_assume(threads_total >= 0);

  pthread_t *tids = malloc(threads_total * sizeof(pthread_t));
  int *is = malloc(threads_total * sizeof(int));

  // create threads
  for (int i = 0; i < threads_total; i++) {
    is[i] = i; // NORACE
    pthread_create(&tids[i], NULL, &thread, &is[i]); // may fail but doesn't matter
  }

  // join threads
  for (int i = 0; i < threads_total; i++) {
    pthread_join(tids[i], NULL);
  }

  free(tids);
  free(is);

  return 0;
}
