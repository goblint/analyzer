// PARAM: --enable ana.int.interval
#include <pthread.h>
#include <assert.h>

int g = 6;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int x = 1;
  pthread_mutex_lock(&A);
  assert(g == 6);
  assert(x == 1);
  g = 5;
  assert(g == 5);
  assert(x == 1);
  pthread_mutex_lock(&B);
  assert(g == 5);
  assert(x == 1);
  pthread_mutex_unlock(&B);
  assert(g == 5);
  assert(x == 1);
  x = g;
  assert(x == 5);
  g = x + 1;
  assert(g == 6);
  x = g;          // added
  assert(g == 6); // added
  assert(x == 6); // added
  pthread_mutex_unlock(&A);
  assert(x == 6); // modified
  return NULL;
}

int main(void) {
  pthread_t id;
  assert(g == 6);
  pthread_create(&id, NULL, t_fun, NULL);
  assert(5 <= g);
  assert(g <= 6);
  pthread_join(id, NULL);
  return 0;
}
