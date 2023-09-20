// POSIX semaphore used as a mutex.
// Extracted from concrat/ProcDump-for-Linux.
#include <stdlib.h>
#include <pthread.h>
#include <semaphore.h>
#include <goblint.h>
extern int __VERIFIER_nondet_int();

int data = 0;
sem_t data_sem;

void *thread(void *arg) {
  sem_wait(&data_sem);
  data = __VERIFIER_nondet_int(); // NORACE
  sem_post(&data_sem);
  return NULL;
}

int main() {
  sem_init(&data_sem, 0, 1);

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
