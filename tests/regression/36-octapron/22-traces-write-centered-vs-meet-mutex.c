// SKIP PARAM: --sets ana.activated[+] octApron
#include <pthread.h>
#include <assert.h>

int g = 0;
int h = 0;
int i = 0;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER; // h <= g
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER; // h == g
pthread_mutex_t C = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int x, y, z;
  pthread_mutex_lock(&C);
  pthread_mutex_lock(&A);
  x = g;
  y = h;
  assert(y <= x);
  pthread_mutex_lock(&B);
  assert(x == y); // TODO (mutex-meet succeeds, write unknown)
  pthread_mutex_unlock(&B);
  i = x + 31;
  z = i;
  assert(z >= x); // TODO (write succeeds, mutex-meet unknown)
  pthread_mutex_unlock(&A);
  pthread_mutex_unlock(&C);
  return NULL;
}

int main(void) {
  int x; // rand

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&B);
  pthread_mutex_lock(&A);
  i = 11;
  g = x;
  h = x - 17;
  pthread_mutex_unlock(&A);
  pthread_mutex_lock(&A);
  h = x;
  pthread_mutex_unlock(&A);
  pthread_mutex_unlock(&B);
  pthread_mutex_lock(&C);
  i = 3;
  pthread_mutex_unlock(&C);
  return 0;
}
