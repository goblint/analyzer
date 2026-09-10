// PARAM: --enable ana.int.interval --set ana.activated[+] thread --set ana.activated[+] threadJoins --set ana.activated[+] threadJoinsPool
#include <pthread.h>

#define N 6

int data;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *worker(void *arg) {
  pthread_mutex_lock(&mutex);
  data++; // NORACE
  pthread_mutex_unlock(&mutex);
  return NULL;
}

int main(void) {
  pthread_t first[N];
  pthread_t second[N];
  int i;

  for (i = 0; i < N; i++) {
    pthread_create(&first[i], NULL, worker, NULL);
    pthread_create(&second[i], NULL, worker, NULL);
  }

  for (i = 0; i < N; i++) {
    pthread_join(first[i], NULL);
    pthread_join(second[i], NULL);
  }

  data++; // NORACE
  return 0;
}
