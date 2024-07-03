// PARAM: --enable ana.int.interval --enable solvers.td3.narrow-sides.enabled --enable solvers.td3.narrow-sides.eliminate-dead --enable ana.base.priv.protection.changes-only
#include <pthread.h>
#include <goblint.h>
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
int a = 0;
int b = 0;

void g(int i) {
  __goblint_check(i <= 10);
  pthread_mutex_lock(&mutex);
  b = i;
  pthread_mutex_unlock(&mutex);
}

void* f(void *d) {
  int j = 0;
  for(int i = 0; i < 10; i++) {
    g(j);
    j = i;
  }
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, f, NULL);
  __goblint_check(b <= 10);
  return 0;
}