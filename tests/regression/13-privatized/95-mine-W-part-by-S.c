// PARAM: --set ana.base.privatization mine-W
#include <pthread.h>
#include <goblint.h>

int g, h;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&A);
  g = 5;
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&B);
  // __goblint_check(g == 5); // UNKNOWN! (data race, so weak read)
  h = 6;
  pthread_mutex_unlock(&B);
  return NULL;
}

int main() {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&A);
  g = 8;
  pthread_mutex_lock(&B);
  __goblint_check(g == 8);
  pthread_mutex_unlock(&B);
  pthread_mutex_unlock(&A);
  return 0;
}
