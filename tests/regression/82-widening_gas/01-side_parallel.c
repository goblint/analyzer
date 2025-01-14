// PARAM: --set solvers.td3.side_widen always --set solvers.td3.side_widen_gas 4 --enable ana.int.interval
#include <pthread.h>
#include <goblint.h>

int a = 0;
int b = 0;
int c = 0;

pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *increase_to_3(void *arg) {
  for(int i = 0; i < 3; i++) {
    pthread_mutex_lock(&A);
    a = i;
    b = i;
    c = i;
    pthread_mutex_unlock(&A);
  }
  return NULL;
}

void *increase_to_4(void *arg) {
  for(int i = 0; i < 4; i++) {
    pthread_mutex_lock(&A);
    b = i;
    c = i;
    pthread_mutex_unlock(&A);
  }
  return NULL;
}

int main(void) {
  // don't care about id
  pthread_t id;
  pthread_create(&id, NULL, increase_to_3, NULL);
  pthread_create(&id, NULL, increase_to_4, NULL);

  pthread_mutex_lock(&A);
  __goblint_check(a >= 0);
  __goblint_check(a <= 3);

  __goblint_check(b >= 0);
  __goblint_check(b <= 4);
  pthread_mutex_unlock(&A);

  return 0;
}
