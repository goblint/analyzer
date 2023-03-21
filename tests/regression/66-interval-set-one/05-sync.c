// PARAM: --set ana.path_sens[+] threadflag --set ana.base.privatization mutex-meet-tid --enable ana.int.interval_set --set ana.activated[-] threadJoins
// Inspired by 36/87
#include <pthread.h>
#include <stdio.h>
#include <goblint.h>

int g;
int h;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex);
  __goblint_check(g==h);
  pthread_mutex_unlock(&mutex);
  return NULL;
}


int main(void) {
  int top2;


  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&mutex);
  if(top2) {
    g=34;
    h=77;
  }

  g=0;
  h=0;
  pthread_mutex_unlock(&mutex);

  pthread_mutex_lock(&mutex);
  __goblint_check(g==h);
  pthread_mutex_unlock(&mutex);

  return 0;
}
