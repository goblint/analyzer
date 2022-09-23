<<<<<<< HEAD
// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --sets ana.relation.privatization mutex-meet-tid
=======
// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.apron.privatization mutex-meet-tid
>>>>>>> master
#include <pthread.h>
#include <assert.h>

int g = 10;
int h = 10;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_benign(void *arg) {
  return NULL;
}

void *t_more(void *arg) {
  // t_more is started multiple times, assert does not need to hold
  pthread_mutex_lock(&A);
  __goblint_check(g == h); //UNKNOWN!
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  g = 12;
  h = 14;
  pthread_mutex_unlock(&A);
  return NULL;
}

void *t_fun(void *arg) {
  // t_more has not been started yet
  pthread_mutex_lock(&A);
  __goblint_check(g == h);
  pthread_mutex_unlock(&A);

  pthread_t more[10];

  for(int i = 0; i <10;i++) {
    pthread_create(&more[i], NULL, t_more, NULL);
    pthread_mutex_lock(&A);
    __goblint_check(g == h); //UNKNOWN!
    pthread_mutex_unlock(&A);
  }

  pthread_mutex_lock(&A);
  __goblint_check(g == h); //UNKNOWN!
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  g = 12;
  h = 14;
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  int t;

  // Force multi-threaded handling
  pthread_t id2;
  pthread_create(&id2, NULL, t_benign, NULL);

  pthread_mutex_lock(&A);
  g = t;
  h = t;
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  __goblint_check(g == h);
  pthread_mutex_unlock(&A);

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&A);
  __goblint_check(g == h); //UNKNOWN!
  pthread_mutex_unlock(&A);

  return 0;
}
