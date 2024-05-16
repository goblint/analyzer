// PARAM: --enable solvers.td3.divided-narrow --enable ana.int.interval
#include <pthread.h>
#include <goblint.h>
#include <unistd.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
int a = 0;
int b = 0;
int c = 0;

void f(void *) {
    for(int i = 0; i < 15; i++) {
      pthread_mutex_lock(&mutex);
      c = i;
      pthread_mutex_unlock(&mutex);
      for(int j = 0; j < i; j++) {
        pthread_mutex_lock(&mutex);
        b = i + j;
        pthread_mutex_unlock(&mutex);
        for(int k = 0; k < j; k++) {
          pthread_mutex_lock(&mutex);
          a = i + j + k;
          pthread_mutex_unlock(&mutex);
          __goblint_check(i + j + k <= 100);
        }
      }
    }
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, f, NULL);

  pthread_mutex_lock(&mutex);
  __goblint_check(a <= 100);
  __goblint_check(b <= 100);
  __goblint_check(c <= 100);
  pthread_mutex_unlock(&mutex);
  return 0;
}
