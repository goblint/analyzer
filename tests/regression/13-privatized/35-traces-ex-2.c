#include <pthread.h>
#include <goblint.h>

int g = 0; // matches expected precise read
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&A);
  pthread_mutex_lock(&B);
  g = 1;
  pthread_mutex_unlock(&B);
  pthread_mutex_lock(&B);
  g = 0;
  pthread_mutex_unlock(&B);
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&B);
  pthread_mutex_lock(&A);
  __goblint_check(g == 0);
  pthread_mutex_unlock(&A);
  pthread_mutex_unlock(&B);
  return 0;
}
