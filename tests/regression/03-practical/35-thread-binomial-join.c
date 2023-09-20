// Thread pool dynamic array binomial heap joining.
// Extracted from concrat/fzy.

/*
  Threads are joined in the shape of a binomial heap:

  0_________________ ...
  |\ \   \          \
  1 2 4   8___       16 ...
    | |\  |\  \      ...
    3 5 6 9 10 12
        |   |  | \
        7   11 13 14
                  |
                  15
*/
#include <stdlib.h>
#include <pthread.h>
#include <goblint.h>
extern int __VERIFIER_nondet_int();

int threads_total;
pthread_t *tids;

int data = 0;
pthread_mutex_t data_mutex = PTHREAD_MUTEX_INITIALIZER;

void *thread(void *arg) {
  int i = arg;
  pthread_mutex_lock(&data_mutex);
  data = __VERIFIER_nondet_int(); // NORACE
  pthread_mutex_unlock(&data_mutex);

  // join threads thread-recursively like binomial heap
  // From original fzy: Fan-in, merging results
  for(unsigned int step = 0;; step++) {
    if (i % (2 << step))
      break;

    unsigned int next_worker = i | (1 << step);
    if (next_worker >= threads_total)
      break;

    pthread_join(tids[next_worker], NULL);
  }
  return NULL;
}

int main() {
  threads_total = __VERIFIER_nondet_int();
  __goblint_assume(threads_total >= 0);

  tids = malloc(threads_total * sizeof(pthread_t));

  // create threads
  // From original fzy: These must be created last-to-first to avoid a race condition when fanning in
  for (int i = threads_total; i >= 0; i--) {
    pthread_create(&tids[i], NULL, &thread, i); // may fail but doesn't matter
  }

  // join threads thread-recursively like binomial heap
  pthread_join(tids[0], NULL);

  free(tids);

  return data; // NORACE (all threads joined)
}
