// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --sets ana.apron.privatization mutex-meet-tid
#include <pthread.h>
#include <assert.h>

int g = 10;
int h = 10;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&A);
  assert(g == h); //UNKNOWN!
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  int t;

  g = 12;
  h = 14;

  pthread_mutex_lock(&A);
  assert(g == h); //FAIL
  pthread_mutex_unlock(&A);

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&A);
  assert(g == h); //FAIL
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  g = t;
  h = t;
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  assert(g == h);
  pthread_mutex_unlock(&A);

  return 0;
}
