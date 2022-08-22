// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --enable ana.sv-comp.functions
extern int __VERIFIER_nondet_int();

#include <pthread.h>
#include <assert.h>

int g = 1;
int h = 1;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  int x = __VERIFIER_nondet_int(); // rand
  pthread_mutex_lock(&A);
  g = x;
  h = x;
  __goblint_check(g == h);
  pthread_mutex_unlock(&A);
  pthread_mutex_lock(&A);
  pthread_mutex_unlock(&A);
  return NULL;
}

void *t2_fun(void *arg) {
  int x = __VERIFIER_nondet_int(); // rand
  int y = __VERIFIER_nondet_int(); //rand
  pthread_mutex_lock(&A);
  g = x;
  h = x;
  __goblint_check(g == h);
  pthread_mutex_unlock(&A);
  pthread_mutex_lock(&A);
  pthread_mutex_unlock(&A);
  pthread_mutex_lock(&A);
  if (y)
    g = x;
  else
    h = x;
  __goblint_check(g == h); // UNKNOWN!
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  pthread_t id, id2;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_create(&id2, NULL, t2_fun, NULL);

  __goblint_check(g == h); // UNKNOWN!
  pthread_mutex_lock(&A);
  __goblint_check(g == h); // UNKNOWN!
  pthread_mutex_unlock(&A);
  return 0;
}
