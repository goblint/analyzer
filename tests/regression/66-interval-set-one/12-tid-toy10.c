// PARAM: --set ana.path_sens[+] threadflag --set ana.base.privatization mutex-meet-tid --enable ana.int.interval_set --set ana.activated[+] threadJoins
// Inspired by 36/80
#include <pthread.h>
#include <goblint.h>

int g = 10;
int h = 10;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_benign(void *arg) {
  pthread_mutex_lock(&A);
  g = 10;
  h = 10;
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  int t = 10;

  // Force multi-threaded handling
  pthread_t id2;
  pthread_create(&id2, NULL, t_benign, NULL);

  pthread_mutex_lock(&A);
  g = 12;
  h = 14;
  pthread_mutex_unlock(&A);

  pthread_join(id2, NULL);

  pthread_mutex_lock(&A);
  __goblint_check(g == h); //UNKNOWN!
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  g = 12;
  h = 14;
  pthread_mutex_unlock(&A);

  int t = 9;

  pthread_mutex_lock(&A);
  __goblint_check(g == h); //FAIL
  pthread_mutex_unlock(&A);

  return 0;
}
