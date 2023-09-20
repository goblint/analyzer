// Thread-local variable flow-sensitive value analysis with dynamic allocation.
// Extracted from silver searcher.
#include <stdlib.h>
#include <pthread.h>
#include <goblint.h>
extern int __VERIFIER_nondet_int();

__thread int* data = NULL;

void *thread(void *arg) {
  int n = __VERIFIER_nondet_int();
  __goblint_assume(n >= 0);

  data = calloc(n, sizeof(int)); // NORACE

  for (int i = 0; i < n; i++) {
    __goblint_check(data[i] == 0); // NORACE
  }

  for (int i = 0; i < n; i++) {
    data[i] = 1; // NORACE
  }

  free(data); // NORACE
  return NULL;
}

int main() {
  int threads_total = __VERIFIER_nondet_int();
  __goblint_assume(threads_total >= 0);

  pthread_t *tids = malloc(threads_total * sizeof(pthread_t));

  // create threads
  for (int i = 0; i < threads_total; i++) {
    pthread_create(&tids[i], NULL, &thread, NULL); // may fail but doesn't matter
  }

  // join threads
  for (int i = 0; i < threads_total; i++) {
    pthread_join(tids[i], NULL);
  }

  free(tids);

  return 0;
}
