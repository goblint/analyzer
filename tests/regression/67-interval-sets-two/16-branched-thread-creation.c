// PARAM: --set ana.path_sens[+] threadflag --set ana.base.privatization mutex-meet-tid --enable ana.int.interval_set --set ana.activated[-] threadJoins
// Inspired by 36/86
#include <pthread.h>
#include <stdio.h>
#include <goblint.h>

int g;
int h;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int t = 4;

  pthread_mutex_lock(&mutex);
  g=t;
  h=t;
  pthread_mutex_unlock(&mutex);
  return NULL;
}


int main(void) {
  int top;
  int mt = 0;

  if(top) {

    g = 8;
    h = 7;

  } else {
    pthread_t id;
    pthread_create(&id, NULL, t_fun, NULL);

    pthread_mutex_lock(&mutex);
    g=top;
    h=top;
    pthread_mutex_unlock(&mutex);
    mt=1;
  }

  if(!mt) {
    pthread_mutex_lock(&mutex);
    __goblint_check(g==h); //MAYFAIL
    pthread_mutex_unlock(&mutex);
  }


  return 0;
}
