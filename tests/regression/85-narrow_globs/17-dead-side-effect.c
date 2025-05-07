// PARAM: --enable solvers.td3.narrow-globs.enabled --enable ana.int.interval --enable solvers.td3.narrow-globs.eliminate-dead --enable ana.base.priv.protection.changes-only
#include <pthread.h>
#include <goblint.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
int a = 0;
int b = 0;

void* f(void *d) {
  pthread_mutex_lock(&mutex);
  if (a < 10) {
    a++;
  }
  if (a > 20) {
    b = 1;
  }
  pthread_mutex_unlock(&mutex);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, f, NULL);
  __goblint_check(!b); //NORACE
  return 0;
}