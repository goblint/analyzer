// PARAM: --set solvers.td3.side_widen never --enable ana.int.interval --set ana.base.privatization protection --set witness.yaml.invariant-types[*] location_invariant --set "ana.activated[+]" unassume --set witness.yaml.unassume 62-tm-inv-transfer-protection-witness.yml --enable ana.widen.tokens
#include <pthread.h>
#include <goblint.h>

int g = 40; // matches expected precise read
pthread_mutex_t B = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  pthread_mutex_lock(&B);
  g = 42;
  g = 41;
  pthread_mutex_unlock(&B);
  return NULL;
}

void *t_fun2(void *arg) {
  g = 41;
  return NULL;
}

int main(void) {
  pthread_t id, id2;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_create(&id2, NULL, t_fun2, NULL);

  pthread_mutex_lock(&B);
  __goblint_check(g >= 40);
  __goblint_check(g <= 41); // UNKNOWN (lacks expressivity)
  pthread_mutex_unlock(&B);

  __goblint_check(g >= 40);
  __goblint_check(g <= 42);

  return 0;
}