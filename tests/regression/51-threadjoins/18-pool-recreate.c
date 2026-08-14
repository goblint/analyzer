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

  for (i = 0; i < N; i++)
    pthread_create(&ids[i], NULL, worker, NULL);
  for (i = 0; i < N; i++)
    pthread_join(ids[i], NULL);

  // A new instance at the same creation site makes the pool live again.
  pthread_create(&ids[0], NULL, worker, NULL);

  data++; // RACE!
  return 0;
}
