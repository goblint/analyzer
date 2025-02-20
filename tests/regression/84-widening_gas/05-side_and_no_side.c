// PARAM: --set solvers.td3.side_widen always --set solvers.td3.side_widen_gas 3 --set solvers.td3.widen_gas 4 --enable ana.int.interval
#include <pthread.h>
#include <goblint.h>

int a = 0;

pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *thread(void *arg) {
  // i: [0, 0] -> [0, 1] -> [0, 2] -> [0, 3]
  // a: [0, 0] -> [0, 1] -> [0, 2]
  for(int i = 0; i != 3; i++) {
    pthread_mutex_lock(&A);
    a = i;
    pthread_mutex_unlock(&A);
  }
  return NULL;
}

int main(void) {
  // don't care about id
  pthread_t id;
  pthread_create(&id, NULL, thread, NULL);

  pthread_mutex_lock(&A);
  __goblint_check(a >= 0);
  __goblint_check(a <= 2);
  pthread_mutex_unlock(&A);

  return 0;
}

