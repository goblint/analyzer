#include <pthread.h>
#include "racemacros.h"

int global1 = 0;
int global2 = 0;

pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void munge(pthread_mutex_t *m, int *v) {
  pthread_mutex_lock(m);
  access_or_assert_racefree(*v); // UNKNOWN
  pthread_mutex_unlock(m);
}

void *t1_fun(void *arg) {
  munge(&mutex1, &global1);
  return NULL;
}

void *t2_fun(void *arg) {
  munge(&mutex2, &global1);
  return NULL;
}

int main(void) {
  create_threads(t1); create_threads(t2);
  join_threads(t1); join_threads(t2);
  return 0;
}