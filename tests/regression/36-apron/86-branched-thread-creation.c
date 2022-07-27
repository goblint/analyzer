// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[-] threadJoins
#include <pthread.h>
#include <stdio.h>
#include <assert.h>

int g;
int h;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int t;

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
    assert(g==h); //MAYFAIL
    pthread_mutex_unlock(&mutex);
  }


  return 0;
}
