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
  create_threads(t);

  while(pthread_mutex_trylock(&mutex)){
    // wait for it
  }
  assert_racefree(global);
  pthread_mutex_unlock(&mutex);
  join_threads(t);
  return 0;
}