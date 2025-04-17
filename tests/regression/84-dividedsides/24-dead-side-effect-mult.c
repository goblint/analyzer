// PARAM: --set ana.context.gas_value 0 --enable solvers.td3.narrow-globs.eliminate-dead  --enable solvers.td3.narrow-globs.eliminate-cyclic-dead --enable solvers.td3.narrow-globs.enabled --enable ana.int.interval  --enable ana.base.priv.protection.changes-only
#include <pthread.h>
#include <goblint.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
int a = 0;
int b = 0;

void annoy() {
  b = 1;
  int top;
  if(b == 1 && top) {
    annoy();
  }
}

void* f(void *d) {
  int top;
  pthread_mutex_lock(&mutex);
  if (a < 10) {
    a++;
  }
  if (a > 20) {
    for(int i =0; i < 100; i++) {
      annoy();
      int j = 4;
      annoy();
      j = 3;
    }
  }

  pthread_mutex_unlock(&mutex);
  return NULL;
}


int main(void) {
  pthread_t id,id1;
  pthread_create(&id, NULL, f, NULL);

  __goblint_check(!b); //NORACE
  return 0;
}
