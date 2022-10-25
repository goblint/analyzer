// modified from 27/09
#include <goblint.h>
#include <pthread.h>

int a = 1;
int b = 1;

pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void* t_fun(void *arg) {
  return NULL;
}

int main() {
  int *x;
  int rnd;

  if (rnd)
    x = &a;
  else
    x = &b;

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL); // go multithreaded

  pthread_mutex_lock(&A); // continue with protected (privatized) values

  __goblint_check(*x == 1);
  b = 2;

  __goblint_check(a == 1);
  if (*x != 0) { // invariant shouldn't make less precise!
    __goblint_check(a == 1);
  }
  return 0;
}