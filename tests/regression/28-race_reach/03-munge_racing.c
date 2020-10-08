#include <pthread.h>
#include "racemacros.h"

int global = 0;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void munge(pthread_mutex_t *m) {
  pthread_mutex_lock(m);
  access(global);
  pthread_mutex_unlock(m);
}

void *t_fun(void *arg) {
  munge(&mutex2);
  return NULL;
}


int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  munge(&mutex1);
  pthread_join (id, NULL);
  assert_racefree(global); // UNKNOWN
  return 0;
}