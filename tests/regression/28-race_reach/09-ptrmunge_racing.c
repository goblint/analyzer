#include <pthread.h>
#include "racemacros.h"

int global1 = 0;
int global2 = 0;

pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void munge(pthread_mutex_t *m, int *v) {
  pthread_mutex_lock(m);
  access(*v);
  pthread_mutex_unlock(m);
}

void *t_fun(void *arg) {
  munge(&mutex1, &global1);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  munge(&mutex2, &global1);
  pthread_join (id, NULL);
  assert_racefree(global1); // UNKNOWN
  assert_racefree(global2);
  return 0;
}