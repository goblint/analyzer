// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
#include <pthread.h>
#include <assert.h>

int g = 1;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&A);
  g = 2; // write something non-initial so base wouldn't find success
  assert(g == 2);
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  int x, y;

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&A);
  x = g;
  y = g;
  assert(x == y);
  pthread_mutex_unlock(&A);

  // g = g - g - x;
  return 0;
}
