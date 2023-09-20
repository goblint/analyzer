// Thread pool constant array sequential joining.
// Extracted from concrat/level-ip.
#include <pthread.h>
#include <goblint.h>

int data = 0;
pthread_mutex_t data_mutex = PTHREAD_MUTEX_INITIALIZER;

void *thread(void *arg) {
  pthread_mutex_lock(&data_mutex);
  data = __VERIFIER_nondet_int(); // NORACE
  pthread_mutex_unlock(&data_mutex);
  return NULL;
}

int main() {
  int threads_total = 4;
  pthread_t tids[4];

  // create threads
  for (int i = 0; i < threads_total; i++) {
    pthread_create(&tids[i], NULL, &thread, NULL); // may fail but doesn't matter
  }

  // join threads
  for (int i = 0; i < threads_total; i++) {
    pthread_join(tids[i], NULL);
  }

  return data; // NORACE (all threads joined)
}
