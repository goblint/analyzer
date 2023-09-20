// Per-thread array index using counter increment.
// Extracted from concrat/ProcDump-for-Linux.
#include <stdlib.h>
#include <pthread.h>
#include <strings.h>
#include <goblint.h>
extern int __VERIFIER_nondet_int();

int *datas;

int next_j = 0;
pthread_mutex_t next_j_mutex = PTHREAD_MUTEX_INITIALIZER;

void *thread(void *arg) {
  int j;
  pthread_mutex_lock(&next_j_mutex);
  j = next_j; // NORACE
  next_j++; // NORACE
  pthread_mutex_unlock(&next_j_mutex);

  datas[j] = __VERIFIER_nondet_int(); // NORACE
  return NULL;
}

int main() {
  int threads_total = __VERIFIER_nondet_int();
  __goblint_assume(threads_total >= 0);
  __goblint_assume(threads_total < 32); // can't have more threads than bits in mask

  pthread_t *tids = malloc(threads_total * sizeof(pthread_t));
  datas = malloc(threads_total * sizeof(int));

  // create threads
  for (int i = 0; i < threads_total; i++) {
    pthread_create(&tids[i], NULL, &thread, NULL); // may fail but doesn't matter
  }

  // join threads
  for (int i = 0; i < threads_total; i++) {
    pthread_join(tids[i], NULL);
  }

  free(tids);
  free(datas);

  return 0;
}
