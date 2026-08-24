// SKIP PARAM: --set ana.activated[+] lin2vareq --set ana.path_sens[+] threadflag --set ana.relation.privatization mutex-meet-tid
// This is basically the same as 36-apron/71-tid-toy1.c, and is the first lin2vareq regression test using privatization
// incorrect handling of is_bot_env and is_top_env can lead this regression test to fail

#include <pthread.h>
#include <goblint.h>

int g = 10;
int h = 10;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int t, r; // rand
  pthread_mutex_lock(&A);
  g = t;
  h = t;
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  int t;

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
