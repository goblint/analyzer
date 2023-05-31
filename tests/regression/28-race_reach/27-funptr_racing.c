#include <pthread.h>
#include <stdio.h>
#include "racemacros.h"

int global;
pthread_mutex_t gm = PTHREAD_MUTEX_INITIALIZER;

void bad() { 
  access(global);
}

void good() { 
  pthread_mutex_lock(&gm);
  access(global);
  pthread_mutex_unlock(&gm);
}

void (*f)() = good;
pthread_mutex_t fm = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  void (*g)();

  pthread_mutex_lock(&fm);
  g = f;
  pthread_mutex_unlock(&fm);

  g();
  return NULL;
}

int main() {
  create_threads(t);

  pthread_mutex_lock(&fm);
  f = bad;
  pthread_mutex_unlock(&fm);

  pthread_mutex_lock(&gm);
  assert_racefree(global); // UNKNOWN
  pthread_mutex_unlock(&gm);

  join_threads(t);
  return 0;
}