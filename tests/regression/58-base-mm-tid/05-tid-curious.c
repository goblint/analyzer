// PARAM: --set ana.path_sens[+] threadflag --set ana.base.privatization mutex-meet-tid --enable ana.int.interval
// Inspired by 36/74
#include <pthread.h>
#include <assert.h>

int g = 10;
int h = 10;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;


void *t_fun(void *arg) {
  pthread_mutex_lock(&A);
  g = 12;
  h = 14;
  // Missing unlock(!)
  return NULL;
}

int main(void) {
  int t;

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&A);
  __goblint_check(g == h); //This succeeds as it can only be entered before t_fun grabs &A and never releases it again
  pthread_mutex_unlock(&A);

  return 0;
}
