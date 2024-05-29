// PARAM: --enable solvers.td3.divided-narrow.enable --enable ana.int.interval --enable ana.base.priv.protection.changes-only
#include <pthread.h>
#include <goblint.h>
#include <unistd.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
int a = 0;

void f(void *) {
    pthread_mutex_lock(&mutex);
    if (a < 10)
        a++;
    pthread_mutex_unlock(&mutex);
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, f, NULL);

  __goblint_check(a <= 10);
  pthread_mutex_lock(&mutex);
  __goblint_check(a <= 10);
  pthread_mutex_unlock(&mutex);
  return 0;
}
