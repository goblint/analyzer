#include <pthread.h>
#include "racemacros.h"

int global = 0;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex1);
  access(global);
  pthread_mutex_unlock(&mutex1);
  return NULL;
}

int main(void) {
  create_threads(t);
  pthread_mutex_lock(&mutex2);
  assert_racefree(global);  // UNKNOWN
  pthread_mutex_unlock(&mutex2);
  join_threads(t);
  return 0;
}