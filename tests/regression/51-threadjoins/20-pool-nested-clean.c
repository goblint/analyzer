// PARAM: --enable ana.int.interval --set ana.activated[+] thread --set ana.activated[+] threadJoins --set ana.activated[+] threadJoinsPool
#include <pthread.h>

#define N 6

int data;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *grandchild(void *arg) {
  pthread_mutex_lock(&mutex);
  data++; // NORACE
  pthread_mutex_unlock(&mutex);
  return NULL;
}

void *worker(void *arg) {
  pthread_t child;
  pthread_create(&child, NULL, grandchild, NULL);
  pthread_join(child, NULL);
  return NULL;
}

int main(void) {
  pthread_t workers[N];
  int i;

  for (i = 0; i < N; i++)
    pthread_create(&workers[i], NULL, worker, NULL);
  for (i = 0; i < N; i++)
    pthread_join(workers[i], NULL);

  data++; // NORACE
  return 0;
}
