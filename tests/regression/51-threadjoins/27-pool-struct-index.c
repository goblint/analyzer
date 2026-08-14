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
  struct {
    int index;
  } cursor;

  for (cursor.index = 0; cursor.index < N; cursor.index++)
    pthread_create(&ids[cursor.index], NULL, worker, NULL);

  // Offset induction variables are unsupported rather than tracked with a
  // stale partition pivot.
  for (cursor.index = 0; cursor.index < N - 1; cursor.index++)
    pthread_join(ids[cursor.index], NULL);

  data++; // RACE!
  return 0;
}
