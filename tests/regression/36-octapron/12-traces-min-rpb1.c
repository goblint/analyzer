// PARAM: --sets ana.activated[+] octApron
#include <pthread.h>
#include <assert.h>

int g = 1;
int h = 1;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int x; // rand
  pthread_mutex_lock(&A);
  g = x;
  h = x;
  assert(g == h);
  pthread_mutex_unlock(&A);
  pthread_mutex_lock(&A);
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  assert(g == h); // UNKNOWN!
  pthread_mutex_lock(&A);
  assert(g == h);
  pthread_mutex_unlock(&A);
  return 0;
}
