// SKIP PARAM: --sets ana.activated[+] octApron --sets exp.solver.td3.side_widen cycle_self
// requires cycle_self to pass
#include <pthread.h>
#include <assert.h>

int g = 42;
int h = 42;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int x; // rand
  pthread_mutex_lock(&B);
  pthread_mutex_lock(&A);
  g = x;
  h = x - 17;
  pthread_mutex_unlock(&A);
  pthread_mutex_lock(&A);
  h = x;
  pthread_mutex_unlock(&A);
  pthread_mutex_unlock(&B);
  return NULL;
}

void *t2_fun(void *arg) {
  int x, y;
  pthread_mutex_lock(&A);
  x = g;
  y = h;
  pthread_mutex_unlock(&A);
  assert(y <= x); // requires cycle_self
  return NULL;
}

void *t3_fun(void *arg) {
  int x, y;
  pthread_mutex_lock(&B);
  pthread_mutex_lock(&A);
  x = g;
  y = h;
  pthread_mutex_unlock(&A);
  pthread_mutex_unlock(&B);
  assert(y == x);
  return NULL;
}

int main(void) {
  int x, y;

  pthread_t id, id2, id3;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_create(&id2, NULL, t2_fun, NULL);
  pthread_create(&id3, NULL, t3_fun, NULL);

  // thread 4
  pthread_mutex_lock(&A);
  x = g;
  y = h;
  pthread_mutex_lock(&B);
  assert(y == x);
  pthread_mutex_unlock(&B);
  pthread_mutex_unlock(&A);
  assert(y == x);
  return 0;
}
