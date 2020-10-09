#include <pthread.h>
#include "racemacros.h"

int global;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex);
  access(global);
  pthread_mutex_unlock(&mutex);

  return NULL;
}

int main(void) {
  create_threads(t_fun);

  pthread_mutex_trylock(&mutex);
  assert_racefree(global); // UNKNOWN
  pthread_mutex_unlock(&mutex);
  join_threads();
  return 0;
}