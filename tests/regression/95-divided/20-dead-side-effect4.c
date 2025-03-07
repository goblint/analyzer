// PARAM: --disable ana.int.def_exc --enable ana.int.interval --set ana.context.gas_value 0 --enable solvers.td3.narrow-sides.enabled --enable solvers.td3.narrow-sides.eliminate-dead --enable ana.base.priv.protection.changes-only
#include <pthread.h>
#include <goblint.h>
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
int b = 0;

void g() {
  pthread_mutex_lock(&mutex);
  b = 1;
  pthread_mutex_unlock(&mutex);
}

void f(int i) {
  if (i < 0) {
    g();
  }
  if (i > 0) {
    f(i - 1);
  }
}

void *thread(void * data) {
  f(10000);
}

int main(void) {
  int id;
  pthread_create(&id, NULL, thread, NULL);
  __goblint_check(b == 0);
  return 0;
}
