#include <pthread.h>
#include "racemacros.h"

int global = 0;
pthread_mutexattr_t mutexattr;
pthread_mutex_t mutex1;
pthread_mutex_t mutex2;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex1);
  access(global);
  pthread_mutex_unlock(&mutex1);
  return NULL;
}

int main(void) {
  pthread_mutexattr_init(&mutexattr);
  pthread_mutexattr_settype(&mutexattr, PTHREAD_MUTEX_ERRORCHECK);
  pthread_mutex_init(&mutex1, &mutexattr);
  pthread_mutex_init(&mutex2, &mutexattr);

  int i = __VERIFIER_nondet_int();
  pthread_mutex_t *m = &mutex1;
  if (i) m = &mutex2;
  create_threads(t);
  pthread_mutex_lock(&mutex1);
  pthread_mutex_unlock(m); // no UB because ERRORCHECK
  assert_racefree(global); // UNKNOWN
  pthread_mutex_unlock(&mutex1); // no UB because ERRORCHECK
  join_threads(t);
  return 0;
}
