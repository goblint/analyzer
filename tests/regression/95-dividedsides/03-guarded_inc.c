// PARAM: --enable solvers.td3.divided-narrow --enable ana.int.interval
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
  pthread_create(&id, NULL, f, NULL);

  __goblint_check(a <= 10);
  pthread_mutex_lock(&mutex);
  __goblint_check(a <= 10);
  pthread_mutex_unlock(&mutex);
  return 0;
}
