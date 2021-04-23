#include <pthread.h>
#include <assert.h>

int g = 17; // matches expected precise read
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t C = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&B);
  pthread_mutex_lock(&C);
  g = 42;
  pthread_mutex_unlock(&B);
  g = 17;
  pthread_mutex_unlock(&C);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&A);
  pthread_mutex_lock(&B);
  pthread_mutex_lock(&C);
  assert(g == 17);
  pthread_mutex_unlock(&A);
  pthread_mutex_unlock(&B);
  pthread_mutex_unlock(&C);
  return 0;
}
