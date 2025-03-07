// PARAM: --enable solvers.td3.narrow-sides.enabled --enable ana.int.interval --enable solvers.td3.narrow-sides.eliminate-dead --enable ana.base.priv.protection.changes-only
#include <pthread.h>
#include <goblint.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
int a = 0;
int b = 0;

void g() {
  b = 1;
}

void* f(void *d) {
  pthread_mutex_lock(&mutex);
  if (a < 10) {
    a++;
  }
  if (a > 20) {
    g();
  }
  pthread_mutex_unlock(&mutex);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, f, NULL);
  pthread_mutex_lock(&mutex);
  __goblint_check(!b);
  pthread_mutex_lock(&mutex);
  return 0;
}
