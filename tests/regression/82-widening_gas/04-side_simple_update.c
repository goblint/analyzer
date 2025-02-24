// PARAM: --set solvers.td3.side_widen always --set solvers.td3.side_widen_gas 3 --enable ana.int.interval --enable exp.earlyglobs
#include <pthread.h>
#include <goblint.h>

int a = 0;
int b = 0;
int c = 0;

pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *thread(void *arg) {
  pthread_mutex_lock(&A);
  a = 1;
  b = 1;
  b = 2;
  c = 1;
  c = 2;
  c = 3;
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  // don't care about id
  pthread_t id;
  pthread_create(&id, NULL, thread, NULL);
  
  pthread_mutex_lock(&A);
  __goblint_check(a <= 1);
  __goblint_check(b <= 2);
  __goblint_check(c <= 3);
  pthread_mutex_unlock(&A);
  return 0;
}
