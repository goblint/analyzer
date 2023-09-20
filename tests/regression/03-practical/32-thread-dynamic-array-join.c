// Thread pool dynamic array sequential joining.
// Extracted from concrat/klib, concrat/minimap2.
// Thread pool from some common library?
#include <stdlib.h>
#include <pthread.h>
#include <goblint.h>
extern int __VERIFIER_nondet_int();

int data = 0;
pthread_mutex_t data_mutex = PTHREAD_MUTEX_INITIALIZER;

void *thread(void *arg) {
  pthread_mutex_lock(&data_mutex);
  data = __VERIFIER_nondet_int(); // NORACE
  pthread_mutex_unlock(&data_mutex);
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

  return data; // NORACE (all threads joined)
}
