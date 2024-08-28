// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set ana.relation.privatization mutex-meet-tid-cluster2 --enable ana.sv-comp.functions
extern int __VERIFIER_nondet_int();

#include <pthread.h>
#include <goblint.h>

int g = 0;
int h = 0;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t2_fun(void *arg) {
  int x;
  pthread_mutex_lock(&A);
  h = 1;
  pthread_mutex_unlock(&A);
  return NULL;
}

void *t3_fun(void *arg) {
  pthread_mutex_lock(&A);
  g = 19;
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  pthread_t id2, id3;
  pthread_create(&id2, NULL, t2_fun, NULL);

  pthread_mutex_lock(&A);
  __goblint_check(g == 0);
  pthread_mutex_unlock(&A);

  pthread_create(&id3, NULL, t3_fun, NULL);
  return 0;
}
