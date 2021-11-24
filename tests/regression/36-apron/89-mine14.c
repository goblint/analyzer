// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[-] threadJoins --enable ana.apron.threshold_widening
// Fig 5a from Miné 2014
#include <pthread.h>
#include <stdio.h>

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
  assert(x <= 100);
  pthread_mutex_unlock(&mutex);
  return 0;
}
