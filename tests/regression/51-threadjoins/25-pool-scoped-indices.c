// PARAM: --enable ana.int.interval --set ana.activated[+] thread --set ana.activated[+] threadJoins --set ana.activated[+] threadJoinsPool
#include <pthread.h>

#define N 8

int data;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *worker(void *arg) {
  pthread_mutex_lock(&mutex);
  data++; // NORACE
  pthread_mutex_unlock(&mutex);
  return NULL;
}

int main(void) {
  pthread_t ids[N];

  for (int create_index = 0; create_index < N; create_index++)
    pthread_create(&ids[create_index], NULL, worker, NULL);

  // The join loop deliberately uses a different scoped induction variable.
  for (int join_index = 0; join_index < N; join_index++)
    pthread_join(ids[join_index], NULL);

  data++; // NORACE
  return 0;
}
