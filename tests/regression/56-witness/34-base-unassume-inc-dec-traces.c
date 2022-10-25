// PARAM: --enable ana.int.interval --set ana.activated[+] unassume --set witness.yaml.unassume 34-base-unassume-inc-dec-traces.yml --set solvers.td3.side_widen always --enable ana.widen.tokens --set ana.base.privatization write+lock
#include <pthread.h>
#include <goblint.h>
int g = 0;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  while (1) {
    pthread_mutex_lock(&A);
    if (g < 10)
      g++;
    pthread_mutex_unlock(&A);
  }
  return NULL;
}

void *t_fun2(void *arg) {
  while (1) {
    pthread_mutex_lock(&A);
    if (g > -10)
      g--;
    pthread_mutex_unlock(&A);
  }
  return NULL;
}

int main() {
  pthread_t id, id2;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_create(&id2, NULL, t_fun2, NULL);

  pthread_mutex_lock(&A);
  __goblint_check(-10 <= g);
  __goblint_check(g <= 10);
  pthread_mutex_unlock(&A);
  return 0;
}
