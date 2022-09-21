// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.activated[-] threadJoins --sets ana.relation.privatization mutex-meet-tid
#include <assert.h>
#include <pthread.h>
#include <stdio.h>

int g;
int h;
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&mutex);
  assert(g==h);
  pthread_mutex_unlock(&mutex);
  return NULL;
}


int main(void) {
  int top, top2;


  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&mutex);
  if(top2) {
    g=34;
    h=77;
  }

  g=top;
  h=top;
  pthread_mutex_unlock(&mutex);

  pthread_mutex_lock(&mutex);
  assert(g==h);
  pthread_mutex_unlock(&mutex);

  return 0;
}
