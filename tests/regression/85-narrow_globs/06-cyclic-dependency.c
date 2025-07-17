// PARAM: --enable solvers.td3.narrow-globs.enabled --enable ana.int.interval
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
    a = b + 1;
    pthread_mutex_unlock(&mutex);
    return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, f, NULL);
  pthread_create(&id, NULL, g, NULL);
  return 0;
}