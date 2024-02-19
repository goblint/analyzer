// PARAM: --enable ana.sv-comp.functions --set ana.activated[+] apron --set ana.relation.privatization mutex-meet-atomic --set sem.int.signed_overflow assume_none
#include <pthread.h>
#include <goblint.h>

extern void __VERIFIER_atomic_begin();
extern void __VERIFIER_atomic_end();

int g = 0;
int h = 0;

pthread_mutex_t m = PTHREAD_MUTEX_INITIALIZER;

void *t_fun(void *arg) {
  __VERIFIER_atomic_begin();
  // pthread_mutex_lock(&m);
  g++;
  h++;
  __VERIFIER_atomic_end();
  // pthread_mutex_unlock(&m);
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  __VERIFIER_atomic_begin();
  // pthread_mutex_lock(&m);
  __goblint_check(g == h);
  __VERIFIER_atomic_end();
  // pthread_mutex_unlock(&m);
  pthread_join (id, NULL);
  return 0;
}
