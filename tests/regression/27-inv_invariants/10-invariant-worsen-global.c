// modified from 27/09
#include <assert.h>
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

  assert(*x == 1);
  b = 2;

  assert(a == 1);
  if (*x != 0) { // TODO: invariant makes less precise!
    assert(a == 1); // TODO
  }
  return 0;
}