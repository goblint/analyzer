// PARAM: --enable ana.int.interval
#include <pthread.h>
#include <assert.h>

int g = 1;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  return NULL;
}

int main(void) {
  int x, r;
  pthread_t id;

  if (r) {
    pthread_create(&id, NULL, t_fun, NULL);

    pthread_mutex_lock(&A);
    g = 5;
    pthread_mutex_unlock(&A);
  }
  else {
    g = 10;
  }

  pthread_mutex_lock(&A);
  x = g; // may read 10!
  __goblint_check(g <= 5); // UNKNOWN!
  pthread_mutex_unlock(&A);
  return 0;
}