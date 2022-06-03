// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --sets ana.relation.privatization mutex-meet-tid-cluster12
extern int __VERIFIER_nondet_int();

#include <pthread.h>
#include <assert.h>

int g = 0;
int h = 0;
pthread_mutex_t A = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&A);
  g = 16;
  pthread_mutex_unlock(&A);
  return NULL;
}

void *t_fun2(void *arg) {
  pthread_mutex_lock(&A);
  h = __VERIFIER_nondet_int();
  h = 12;
  pthread_mutex_unlock(&A);
  return NULL;
}

int main(void) {
  pthread_t id, id2;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_create(&id2, NULL, t_fun2, NULL);

  pthread_mutex_lock(&A);
  h = 31;
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  h = 12;
  pthread_mutex_unlock(&A);

  pthread_mutex_lock(&A);
  int z = h;
  assert(z != 31);
  pthread_mutex_unlock(&A);
  return 0;
}
