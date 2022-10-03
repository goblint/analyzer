// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --enable ana.sv-comp.functions
extern int __VERIFIER_nondet_int();

#include <pthread.h>
#include <goblint.h>

int g = 42;
int h = 42;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int x = __VERIFIER_nondet_int(); //rand
  if (x > -1000) { // avoid underflow
    pthread_mutex_lock(&B);
    pthread_mutex_lock(&A);
    g = x;
    h = x - 17;
    pthread_mutex_unlock(&A);
    pthread_mutex_lock(&A);
    h = x;
    pthread_mutex_unlock(&A);
    pthread_mutex_unlock(&B);
  }
  return NULL;
}

void *t2_fun(void *arg) {
  int x = __VERIFIER_nondet_int(); //rand
  int y = __VERIFIER_nondet_int(); //rand
  pthread_mutex_lock(&A);
  x = g;
  y = h;
  pthread_mutex_unlock(&A);
  __goblint_check(y <= x);
  return NULL;
}

void *t3_fun(void *arg) {
  int x = __VERIFIER_nondet_int(); //rand
  int y = __VERIFIER_nondet_int(); //rand
  pthread_mutex_lock(&B);
  pthread_mutex_lock(&A);
  x = g;
  y = h;
  pthread_mutex_unlock(&A);
  pthread_mutex_unlock(&B);
  __goblint_check(y == x); // TODO (mutex-meet succeeds, protection unknown)
  return NULL;
}

int main(void) {
  int x = __VERIFIER_nondet_int(); //rand
  int y = __VERIFIER_nondet_int(); //rand

  pthread_t id, id2, id3;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_create(&id2, NULL, t2_fun, NULL);
  pthread_create(&id3, NULL, t3_fun, NULL);

  // thread 4
  pthread_mutex_lock(&A);
  x = g;
  y = h;
  pthread_mutex_lock(&B);
  __goblint_check(y == x); // TODO (mutex-meet succeeds, protection unknown)
  pthread_mutex_unlock(&B);
  pthread_mutex_unlock(&A);
  __goblint_check(y == x); // TODO (mutex-meet succeeds, protection unknown)
  return 0;
}
