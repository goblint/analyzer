#include <pthread.h>
#include "racemacros.h"

pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int *p = (int *) arg;
  pthread_mutex_lock(&mutex1);
  access(*p);
  pthread_mutex_unlock(&mutex1);
  return NULL;
}

int main(void) {
  pthread_t id;
  int i = 0;
  pthread_create(&id, NULL, t_fun, (void *) &i);
  pthread_mutex_lock(&mutex1);
  assert_racefree(i);
  pthread_mutex_unlock(&mutex1);
  pthread_join(id, NULL);
  return 0;
}
