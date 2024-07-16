// PARAM: --set ana.path_sens[+] threadflag --set ana.base.privatization mutex-meet-tid --enable ana.int.interval --set ana.activated[+] threadJoins --enable ana.int.interval_threshold_widening
// Fig 5a from Miné 2014
#include <pthread.h>
#include <stdio.h>
#include <goblint.h>

int x;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int top;
  while(top) {
    pthread_mutex_lock(&mutex);
    if(x<100) {
      x++;
    }
    pthread_mutex_unlock(&mutex);
  }
  return NULL;
}


int main(void) {
  int top, top2;


  pthread_t id;
  pthread_t id2;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_create(&id2, NULL, t_fun, NULL);
  pthread_mutex_lock(&mutex);
  __goblint_check(x <= 100);
  pthread_mutex_unlock(&mutex);
  return 0;
}
