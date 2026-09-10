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
  int index;
  int *cursor = &index;

  for (*cursor = 0; *cursor < N; (*cursor)++)
    pthread_create(&ids[*cursor], NULL, worker, NULL);

  // Dereferenced induction variables are likewise rejected conservatively.
  for (*cursor = 0; *cursor < N - 1; (*cursor)++)
    pthread_join(ids[*cursor], NULL);

  data++; // RACE!
  return 0;
}
