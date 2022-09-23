// PARAM: --enable ana.sv-comp.functions
#include <pthread.h>
#include <assert.h>

extern void __VERIFIER_atomic_begin();
extern void __VERIFIER_atomic_end();

int myglobal = 5;

void *t_fun(void *arg) {
  myglobal++;
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  __VERIFIER_atomic_begin();
  __goblint_check(myglobal == 5); // UNKNOWN!
  __VERIFIER_atomic_end();
  pthread_join (id, NULL);
  return 0;
}
