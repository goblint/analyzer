// PARAM: --set ana.path_sens[+] threadflag --sets ana.base.privatization mutex-meet-tid
// Inspired by 36/71
#include <pthread.h>
#include <goblint.h>

int g = 10;
int h = 10;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int t = 13; // rand
  pthread_mutex_lock(&A);
  g = t;
  h = t;
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  int t = 13;

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&A);
  g = 31;
  h = 17;
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  __goblint_check(g == h); //UNKNOWN!
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  g = t;
  h = t;
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  __goblint_check(g == h); // t_fun always has the invariant it only is violated in main temporarily
  pthread_mutex_unlock(&A);

  return 0;
}
