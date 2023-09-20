// Per-thread array index passed via argument.
// Thread pool joining via threads alive counter decremented in cleaner thread.
// Extracted from smtprc.
// TODO: race correctly found here, but not in smtprc.
#include <pthread.h>
#include <goblint.h>
extern int __VERIFIER_nondet_int();

int threads_total;
int threads_alive = 0;
pthread_mutex_t threads_alive_mutex = PTHREAD_MUTEX_INITIALIZER;

pthread_t *tids;
int *datas;

void *thread(void *arg) {
  int i = arg;
  datas[i] = 1; // RACE!
  return NULL;
}

void *cleaner(void *arg) {
  while (1) {
    for (int i = 0; i < threads_total; i++) {
      if (datas[i]) { // RACE!
        pthread_join(tids[i], NULL); // NORACE
        pthread_mutex_lock(&threads_alive_mutex);
        threads_alive--; // NORACE
        pthread_mutex_unlock(&threads_alive_mutex);
        datas[i] = 0; // RACE!
      }
    }
  }
  return NULL;
}

int main() {
  threads_total = __VERIFIER_nondet_int();
  __goblint_assume(threads_total >= 0);

  tids = malloc(threads_total * sizeof(pthread_t));
  datas = calloc(threads_total, sizeof(int));

  // create threads
  pthread_t cleaner_tid;
  pthread_create(&cleaner_tid, NULL, &cleaner, NULL);

  for (int i = 0; i < threads_total; i++) {
    pthread_create(&tids[i], NULL, &thread, i); // NORACE may fail but doesn't matter

    pthread_mutex_lock(&threads_alive_mutex);
    threads_alive++; // NORACE
    pthread_mutex_unlock(&threads_alive_mutex);
  }

  // wait for all threads to stop
  pthread_mutex_lock(&threads_alive_mutex);
  while (threads_alive) { // NORACE
    pthread_mutex_unlock(&threads_alive_mutex);
    // busy loop for simplicity
    pthread_mutex_lock(&threads_alive_mutex);
  }
  pthread_mutex_unlock(&threads_alive_mutex);

  free(tids);

  return datas[0]; // NORACE (all threads stopped)
}
