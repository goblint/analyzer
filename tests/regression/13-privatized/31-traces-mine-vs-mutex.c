// Copied & modified from 13/28.
#include <pthread.h>
#include <assert.h>

int g;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&A);
  pthread_mutex_lock(&B);
  g++;
  pthread_mutex_unlock(&B); // Write Mine influence: [[g, B], t2_fun, {A}] -> 1
  pthread_mutex_lock(&B);
  g--;
  pthread_mutex_unlock(&B); // Write Mine influence: [[g, B], t2_fun, {A}] -> 0
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&B); // Read & join to g Mine influence: [[g, B], t2_fun, {A}] -> (0 join 1 = Unknown)
  pthread_mutex_unlock(&B);

  pthread_mutex_lock(&A);
  pthread_mutex_lock(&B);
  __goblint_check(g == 0);
  pthread_mutex_unlock(&B);
  pthread_mutex_unlock(&A);

  pthread_join(id, NULL);
  return 0;
}