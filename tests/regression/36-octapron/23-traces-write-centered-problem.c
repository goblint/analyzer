// SKIP PARAM: --sets ana.activated[+] octApron
#include <pthread.h>
#include <assert.h>

int g = 25; // matches write in main
int h = 12; // matches write in main
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int x; // rand
  pthread_mutex_lock(&A);
  g = x;
  h = x - 12;
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  int x, y;

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&A);
  g = 25;
  h = 12;
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  x = g;
  y = h;
  assert(x >= y); // write would fail this due to disjunctive reading from local and global
  pthread_mutex_unlock(&A);
  return 0;
}
