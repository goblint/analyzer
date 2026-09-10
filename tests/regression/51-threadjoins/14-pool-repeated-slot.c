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
  pthread_t id;
  int i;

  // One slot cannot cover multiple outstanding dynamic instances.
  for (i = 0; i < N; i++)
    pthread_create(&id, NULL, worker, NULL);

  for (i = 0; i < N; i++)
    pthread_join(id, NULL);

  data++; // RACE!
  return 0;
}
