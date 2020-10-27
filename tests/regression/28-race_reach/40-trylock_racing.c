#include <pthread.h>
#include "racemacros.h"

int global;
pthread_mutexattr_t mutexattr;
pthread_mutex_t mutex;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex);
  access(global);
  pthread_mutex_unlock(&mutex);

  return NULL;
}

int main(void) {
  pthread_mutexattr_init(&mutexattr);
  pthread_mutexattr_settype(&mutexattr, PTHREAD_MUTEX_ERRORCHECK);
  pthread_mutex_init(&mutex, &mutexattr);

  create_threads(t);

  pthread_mutex_trylock(&mutex);
  assert_racefree(global); // UNKNOWN
  pthread_mutex_unlock(&mutex); // no UB because ERRORCHECK
  join_threads(t);
  return 0;
}