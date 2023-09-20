// Race-free due to GCC atomic operation.
// Extracted from concrat/klib.
#include <stdlib.h>
#include <pthread.h>
#include <goblint.h>
extern int __VERIFIER_nondet_int();

int data;

void *thread(void *arg) {
  __sync_fetch_and_add(&data, 1); // NORACE
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
