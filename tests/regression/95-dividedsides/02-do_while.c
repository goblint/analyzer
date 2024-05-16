// PARAM: --enable solvers.td3.divided-narrow --enable ana.int.interval --set solvers.td3.side_widen sides-local
#include <pthread.h>
#include <goblint.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
int a = 0;

void f() {
    pthread_mutex_lock(&mutex);
    __goblint_check(a <= 10);
    pthread_mutex_unlock(&mutex);
}

void h() {
    int i = 0;
    do {
        pthread_mutex_lock(&mutex);
        a = i;
        pthread_mutex_unlock(&mutex);
        i++;
    } while(i < 10);
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, f, NULL);
  pthread_create(&id, NULL, h, NULL);
  return 0;
}
