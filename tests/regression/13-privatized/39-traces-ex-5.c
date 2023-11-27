#include <pthread.h>
#include <goblint.h>

int g = 2; // matches expected synchronized read
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t D = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&D);
  g = 1;
  pthread_mutex_unlock(&D);
  return NULL;
}

void *t2_fun(void *arg) {
  pthread_mutex_lock(&A);
  g = 2;
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  pthread_t id, id2;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_create(&id2, NULL, t2_fun, NULL);

  pthread_mutex_lock(&D);
  pthread_mutex_lock(&A);
  pthread_mutex_unlock(&D);
  __goblint_check(g == 2); // UNKNOWN!
  pthread_mutex_unlock(&A);
  return 0;
}
