// PARAM: --enable ana.int.interval
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
  pthread_mutex_unlock(&A);
  g = 3;
  pthread_mutex_unlock(&B);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&A);
  assert(g >= 2); // TODO
  pthread_mutex_lock(&B);
  assert(g >= 2); // TODO
  assert(g == 3); // TODO
  return 0;
}
