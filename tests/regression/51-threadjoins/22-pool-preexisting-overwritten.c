// PARAM: --enable ana.int.interval --set ana.activated[+] thread --set ana.activated[+] threadJoins --set ana.activated[+] threadJoinsPool
#include <pthread.h>

#define N 8

int data;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *worker(void *arg) {
  pthread_mutex_lock(&mutex);
  data++; // RACE!
  pthread_mutex_unlock(&mutex);
  return NULL;
}

int main(void) {
  pthread_t ids[N];
  int i;

  // This extra instance is the completeness hole in the original POC: the
  // following loop overwrites its only canonical handle.
  pthread_create(&ids[0], NULL, worker, NULL);
  for (i = 0; i < N; i++)
    pthread_create(&ids[i], NULL, worker, NULL);

  for (i = 0; i < N; i++)
    pthread_join(ids[i], NULL);

  data++; // RACE!
  return 0;
}
