// PARAM: --enable solvers.td3.narrow-sides.enabled --enable ana.int.interval
#include <pthread.h>
#include <goblint.h>

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
int a = 0;
int b = 0;
int c = 0;

void f() {
    pthread_mutex_lock(&mutex);
    __goblint_check(a <= 10);
    __goblint_check(b <= 10);
    __goblint_check(c <= 10);
    pthread_mutex_unlock(&mutex);
}

void h() {
    pthread_mutex_lock(&mutex);
    for(a = 0; a < 10; a++) {
        b = a;
    }
    c = a;
    pthread_mutex_unlock(&mutex);
    f();
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, f, NULL);
  pthread_create(&id, NULL, h, NULL);
  return 0;
}
