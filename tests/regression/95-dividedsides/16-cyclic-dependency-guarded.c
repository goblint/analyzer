// PARAM: --enable solvers.td3.narrow-sides.enabled --enable solvers.td3.narrow-sides.stable --set solvers.td3.narrow-sides.narrow-gas 9 --enable ana.int.interval

// This is supposed to check if the solver terminates
#include <pthread.h>
#include <goblint.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
int a = 0;
int b = 0;
void* g(void *) {
    pthread_mutex_lock(&mutex);
    b = a + 1;
    pthread_mutex_unlock(&mutex);
    return NULL;
}


void* f(void *) {
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