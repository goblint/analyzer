#include<pthread.h>
#include "racemacros.h"

int x = 0;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex);
  pthread_mutex_lock(&__global_lock);
  if (x == 0) { // NORACE
    pthread_mutex_unlock(&__global_lock);
    pthread_mutex_unlock(&mutex);
  } else {
    pthread_mutex_unlock(&__global_lock);
    pthread_mutex_unlock(&mutex);
    access(x);
  }
  return NULL;
}

int main(void) {
  create_threads(t);
  access(x);
  pthread_mutex_lock(&mutex);
  assert_racefree(x); // UNKNOWN
  pthread_mutex_unlock(&mutex);
  join_threads(t);
  return 0;
}
