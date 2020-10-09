#include <pthread.h>
#include "racemacros.h"

int g1,g2;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex1);
  access(g1);
  pthread_mutex_unlock(&mutex1);

  pthread_mutex_lock(&mutex2);
  access(g2);
  pthread_mutex_unlock(&mutex2);
  return NULL;
}

int main(void) {
  create_threads(t);

  pthread_mutex_lock(&mutex1);
  assert_racefree(g1);

  while(pthread_mutex_trylock(&mutex2)){
    pthread_mutex_unlock(&mutex1);
    pthread_mutex_lock(&mutex1);
  }
  assert_racefree(g1);
  assert_racefree(g2);
  pthread_mutex_unlock(&mutex2);

  assert_racefree(g1);
  pthread_mutex_unlock(&mutex1);

  join_threads(t);
  return 0;
}