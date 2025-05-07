// PARAM: --enable solvers.td3.narrow-globs.enabled --set solvers.td3.narrow-globs.narrow-gas 9 --enable ana.int.interval --set ana.base.privatization protection-read  --enable ana.base.priv.protection.changes-only
// NOTIMEOUT
#include <pthread.h>
#include <goblint.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
int a = 0;
int b = 0;
void* g(void *d) {
    pthread_mutex_lock(&mutex);
    b = a + 1;
    pthread_mutex_unlock(&mutex);
    return NULL;
}


void* f(void *d) {
    pthread_mutex_lock(&mutex);
    if (b < 10)
      a = b;
    pthread_mutex_unlock(&mutex);
    return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, f, NULL);
  pthread_create(&id, NULL, g, NULL);
  pthread_mutex_lock(&mutex);
  __goblint_check(a <= 10);
  __goblint_check(b <= 10);
  pthread_mutex_unlock(&mutex);
  return 0;
}
