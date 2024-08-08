// PARAM: --enable ana.int.interval --enable solvers.td3.narrow-sides.enabled 
#include <pthread.h>
#include <stdio.h>

int a, b;
pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

void *t_fun1(void *arg) {
  int i = a;
  if (i < 1000) {
    b = i + 1;
  }
}

void *t_fun2(void *arg) {
  int i = b;
  if (i < 1000) {
    a = i + 1;
  }
}

int main(void) {
  pthread_t id1, id2;
  pthread_create(&id1, NULL, t_fun1, NULL);
  pthread_create(&id2, NULL, t_fun2, NULL);
  pthread_join (id1, NULL);
  pthread_join (id2, NULL);
  pthread_mutex_lock(&m);
  __goblint_check(a <= 1000);
  __goblint_check(b <= 1000);
  pthread_mutex_unlock(&m);
  return 0;
}
