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
  pthread_t alias;
  int i;

  for (i = 0; i < N; i++)
    pthread_create(&ids[i], NULL, worker, NULL);

  alias = ids[2];
  pthread_detach(alias);
  for (i = 0; i < N; i++) {
    if (i != 2)
      pthread_join(ids[i], NULL);
  }

  data++; // RACE!
  return 0;
}
