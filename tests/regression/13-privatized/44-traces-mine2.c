#include <pthread.h>
#include <assert.h>

int g = 3; // matches one expected precise read
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&A);
  g = 1;
  pthread_mutex_lock(&B);
  g = 2;
  pthread_mutex_unlock(&B);
  g = 3;
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&A);
  __goblint_check(g == 3);
  pthread_mutex_lock(&B);
  __goblint_check(g == 3);
  return 0;
}
