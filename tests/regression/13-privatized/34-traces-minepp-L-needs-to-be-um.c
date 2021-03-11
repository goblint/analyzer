#include <pthread.h>
#include <assert.h>

int g = 27; // matches expected precise read
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t C = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&A);
  pthread_mutex_lock(&B);
  g = 15;
  pthread_mutex_lock(&C);
  pthread_mutex_unlock(&C);
  g = 27;
  pthread_mutex_unlock(&A);
  pthread_mutex_unlock(&B);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  int r;
  if (r) {
    pthread_mutex_lock(&A);
    pthread_mutex_lock(&C);
    pthread_mutex_unlock(&A);
  }
  else {
    pthread_mutex_lock(&B);
    pthread_mutex_lock(&C);
    pthread_mutex_unlock(&B);
  }
  // mine-w also reads 15 here by weak influence, so useless example
  assert(g == 27);
  return 0;
}
