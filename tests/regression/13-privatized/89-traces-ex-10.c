#include <pthread.h>
#include <goblint.h>

int g = 7; // matches precise read
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&A);
  pthread_mutex_lock(&B);
  g = 42;
  pthread_mutex_unlock(&B);
  g = 7;
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&B);
  // __goblint_check(g == 7); // UNKNOWN!
  pthread_mutex_unlock(&B);

  pthread_mutex_lock(&A);
  pthread_mutex_lock(&B);
  __goblint_check(g == 7);
  return 0;
}
