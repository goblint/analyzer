// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
extern int __VERIFIER_nondet_int();

#include <pthread.h>
#include <assert.h>

int g = 25; // matches write in main
int h = 12; // matches write in main
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int x = __VERIFIER_nondet_int(); //rand
  if (x > -1000) { // avoid underflow
    pthread_mutex_lock(&A);
    g = x;
    h = x - 12;
    pthread_mutex_unlock(&A);
  }
  return NULL;
}

int main(void) {
  int x = __VERIFIER_nondet_int(); //rand
  int y = __VERIFIER_nondet_int(); //rand

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  pthread_mutex_lock(&A);
  g = 25;
  h = 12;
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  x = g;
  y = h;
  assert(x >= y); // write would fail this due to disjunctive reading from local and global
  pthread_mutex_unlock(&A);
  return 0;
}
