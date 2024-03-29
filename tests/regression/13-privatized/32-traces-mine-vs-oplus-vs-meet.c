#include <pthread.h>
#include <goblint.h>

int g = 0;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t C = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&A);
  pthread_mutex_lock(&C);
  pthread_mutex_lock(&B);
  g = 5;
  pthread_mutex_unlock(&B);
  pthread_mutex_lock(&B);
  g = 0;
  pthread_mutex_unlock(&B);
  pthread_mutex_unlock(&C);
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  // This must be before the other to get Mine to fail for the other even with thread ID partitioning.
  pthread_mutex_lock(&B);
  pthread_mutex_lock(&C);
  __goblint_check(g == 0); // TODO (mine and mutex-oplus fail, mutex-meet succeeds)
  pthread_mutex_unlock(&C);
  pthread_mutex_unlock(&B);

  pthread_mutex_lock(&A);
  pthread_mutex_lock(&B);
  __goblint_check(g == 0); // TODO (mine fails, mutex-oplus and mutex-meet succeed)
  pthread_mutex_unlock(&B);
  pthread_mutex_unlock(&A);

  pthread_join(id, NULL);
  return 0;
}
