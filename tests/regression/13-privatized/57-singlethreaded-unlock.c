#include <pthread.h>
#include <assert.h>

int g = 0;

pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  // just for going to multithreaded mode
  return NULL;
}

int main() {
  pthread_mutex_lock(&A);
  g = 1;
  pthread_mutex_unlock(&A); // singlethreaded mode unlock

  g = 2;

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  __goblint_check(g == 2);
  return 0;
}
