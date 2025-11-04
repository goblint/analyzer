// PARAM: --set ana.path_sens[+] threadflag --set ana.base.privatization mutex-meet-tid --enable ana.int.interval
// Inspired by 36/72
#include <pthread.h>
#include <goblint.h>

int g = 10;
int h = 10;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int t = 9; // rand
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
  __goblint_check(g == h); //UNKNOWN! This thread is multiple and needs to read from itself
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  int t;

  pthread_t id[10];
  for(int i = 0; i < 10;i++){
    pthread_create(&id[i], NULL, t_fun, NULL);
  }

  pthread_mutex_lock(&A);
  __goblint_check(g == h); //UNKNOWN!
  pthread_mutex_unlock(&A);

  return 0;
}
