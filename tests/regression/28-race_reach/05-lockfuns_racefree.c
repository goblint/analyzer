#include <pthread.h>
#include "racemacros.h"

int global = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void lock() {
  pthread_mutex_lock(&mutex);
}

void unlock() {
  pthread_mutex_unlock(&mutex);
}

void *t_fun(void *arg) {
  lock();
  access(global);
  unlock();
  return NULL;
}

int main(void) {
  create_threads(t);
  lock();
  assert_racefree(global);
  unlock();
  join_threads(t);
  return 0;
}