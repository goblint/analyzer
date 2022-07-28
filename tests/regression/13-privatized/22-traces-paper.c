// PARAM: --enable ana.int.interval
#include <pthread.h>
#include <assert.h>

int g = 6;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int x = 1;
  pthread_mutex_lock(&A);
  __goblint_check(g == 6);
  __goblint_check(x == 1);
  g = 5;
  __goblint_check(g == 5);
  __goblint_check(x == 1);
  pthread_mutex_lock(&B);
  __goblint_check(g == 5);
  __goblint_check(x == 1);
  pthread_mutex_unlock(&B);
  __goblint_check(g == 5);
  __goblint_check(x == 1);
  x = g;
  __goblint_check(x == 5);
  g = x + 1;
  __goblint_check(g == 6);
  pthread_mutex_unlock(&A);
  __goblint_check(x == 5);
  return NULL;
}

int main(void) {
  pthread_t id;
  __goblint_check(g == 6);
  pthread_create(&id, NULL, t_fun, NULL);
  __goblint_check(5 <= g);
  __goblint_check(g <= 6);
  pthread_join(id, NULL);
  return 0;
}
