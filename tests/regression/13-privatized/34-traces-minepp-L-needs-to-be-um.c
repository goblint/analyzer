extern int __VERIFIER_nondet_int();

#include <pthread.h>
#include <assert.h>

int g = 42; // matches expected precise read
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t C = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&A);
  pthread_mutex_lock(&B);
  pthread_mutex_lock(&C);
  g = 15;
  pthread_mutex_unlock(&C);
  pthread_mutex_lock(&C);
  g = 42;
  pthread_mutex_unlock(&C);
  pthread_mutex_unlock(&B);
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  int r = __VERIFIER_nondet_int();
  if (r) {
    pthread_mutex_lock(&A);
    pthread_mutex_lock(&C);
    pthread_mutex_unlock(&A);
  }
  else {
    pthread_mutex_lock(&B);
    pthread_mutex_lock(&C);
    pthread_mutex_unlock(&B);
  }
  // mine-w also reads 15 here by weak influence, so useless example
  __goblint_check(g == 42); // TODO
  return 0;
}
