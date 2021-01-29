#include <pthread.h>
#include <assert.h>

int g1 = 0;
int g2 = 0;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t C = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&A);
  pthread_mutex_lock(&B);
  g1 = 5;
  pthread_mutex_unlock(&B);
  pthread_mutex_lock(&B);
  g1 = 0;
  pthread_mutex_unlock(&B);
  pthread_mutex_lock(&C);
  g2 = 5;
  pthread_mutex_unlock(&C);
  pthread_mutex_lock(&C);
  g2 = 0;
  pthread_mutex_unlock(&C);
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&B);
  pthread_mutex_unlock(&B);

  pthread_mutex_lock(&A);
  pthread_mutex_lock(&B);
  assert(g1 == 0);
  pthread_mutex_unlock(&B);
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&C);
  pthread_mutex_lock(&A);
  assert(g2 == 0);
  pthread_mutex_unlock(&A);
  pthread_mutex_unlock(&C);

  pthread_join(id, NULL);
  return 0;
}
