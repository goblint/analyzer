#include <pthread.h>
#include "racemacros.h"

int global;
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mutex2 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t * mp = &mutex1;

void *t_fun(void *arg) {
  pthread_mutex_t * mp1;
  int *p = &global;
  mp1 = mp;
  pthread_mutex_lock(mp);
  access(global);
  pthread_mutex_unlock(mp);
  return NULL;
}

int main(void) {
  pthread_mutex_t * mp1;
  mp = &mutex1;
  create_threads(t);
  mp1 = mp;
  pthread_mutex_lock(mp);
  assert_racefree(global);
  pthread_mutex_unlock(mp);
  join_threads(t);
  return 0;
}
