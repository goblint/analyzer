// PARAM: --enable solvers.td3.narrow-sides.enabled --enable ana.int.interval
#include <pthread.h>
#include <goblint.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
int a = 17;

void f() {
  pthread_mutex_lock(&mutex);
  if (a % 2) {
    a = 3 * a + 1;
  } else {
    a = a / 2;
  }
  a = a % 0xbeef;
  pthread_mutex_unlock(&mutex);
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, f, NULL);
  pthread_mutex_lock(&mutex);
  __goblint_check(a >= 0); // UNKNOWN cannot narrow lower bound to 0, only -0xbeef + 1
  __goblint_check(a < 0xbeef);
  pthread_mutex_unlock(&mutex);
  return 0;
}
