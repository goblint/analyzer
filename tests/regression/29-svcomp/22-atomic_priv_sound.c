// PARAM: --enable ana.sv-comp.functions
#include <pthread.h>
#include <goblint.h>

extern void __VERIFIER_atomic_begin();
extern void __VERIFIER_atomic_end();

int myglobal = 5;

void *t_fun(void *arg) {
  __VERIFIER_atomic_begin();
  __goblint_check(myglobal == 5); // TODO
  myglobal++;
  __goblint_check(myglobal == 6); // TODO
  __VERIFIER_atomic_end();
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  __goblint_check(myglobal == 5); // UNKNOWN!
  __VERIFIER_atomic_begin();
  __goblint_check(myglobal == 5); // UNKNOWN!
  __VERIFIER_atomic_end();
  pthread_join (id, NULL);
  return 0;
}
