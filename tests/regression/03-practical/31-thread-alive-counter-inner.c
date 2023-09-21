// Thread pool joining via threads alive counter incremented inside of thread.
// Extracted from concrat/C-Thread-Pool.
#include <stdbool.h>
#include <pthread.h>
#include <goblint.h>
extern int __VERIFIER_nondet_int();

int threads_alive = 0;
pthread_mutex_t threads_alive_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t threads_alive_cond = PTHREAD_COND_INITIALIZER;

bool keep_alive = true;
pthread_mutex_t keep_alive_mutex = PTHREAD_MUTEX_INITIALIZER;

int data = 0;
pthread_mutex_t data_mutex = PTHREAD_MUTEX_INITIALIZER;

void *thread(void *arg) {
  pthread_mutex_lock(&threads_alive_mutex);
  threads_alive++; // NORACE
  pthread_cond_signal(&threads_alive_cond);
  pthread_mutex_unlock(&threads_alive_mutex);

  pthread_mutex_lock(&keep_alive_mutex);
  while (keep_alive) { // NORACE
    pthread_mutex_unlock(&keep_alive_mutex);

    pthread_mutex_lock(&data_mutex);
    data = __VERIFIER_nondet_int(); // NORACE
    pthread_mutex_unlock(&data_mutex);

    pthread_mutex_lock(&keep_alive_mutex);
  }
  pthread_mutex_unlock(&keep_alive_mutex);

  pthread_mutex_lock(&threads_alive_mutex);
  threads_alive--; // NORACE
  pthread_cond_signal(&threads_alive_cond);
  pthread_mutex_unlock(&threads_alive_mutex);
  return NULL;
}

int main() {
  int threads_total = __VERIFIER_nondet_int();
  __goblint_assume(threads_total >= 0);

  // create threads
  for (int i = 0; i < threads_total; i++) {
    pthread_t tid;
    pthread_create(&tid, NULL, &thread, NULL); // may fail but doesn't matter
    pthread_detach(tid);
  }

  // wait for all threads to come alive
  pthread_mutex_lock(&threads_alive_mutex);
  while (threads_alive != threads_total) // NORACE
    pthread_cond_wait(&threads_alive_cond, &threads_alive_mutex);
  pthread_mutex_unlock(&threads_alive_mutex);

  // stop threads
  pthread_mutex_lock(&keep_alive_mutex);
  keep_alive = false; // NORACE
  pthread_mutex_unlock(&keep_alive_mutex);

  // wait for all threads to stop
  pthread_mutex_lock(&threads_alive_mutex);
  while (threads_alive) // NORACE
    pthread_cond_wait(&threads_alive_cond, &threads_alive_mutex);
  pthread_mutex_unlock(&threads_alive_mutex);

  return data; // NORACE (all threads stopped)
}
