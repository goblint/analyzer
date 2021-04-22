// PARAM: --enable ana.sv-comp.functions
#include <pthread.h>
#include <assert.h>

extern void __VERIFIER_atomic_begin();
extern void __VERIFIER_atomic_end();

int myglobal = 5;

void *t_fun(void *arg) {
  __VERIFIER_atomic_begin();
  assert(myglobal == 5);
  myglobal++;
  assert(myglobal == 6);
  myglobal--;
  assert(myglobal == 5);
  __VERIFIER_atomic_end();
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  assert(myglobal == 5); // TODO
  __VERIFIER_atomic_begin();
  assert(myglobal == 5);
  __VERIFIER_atomic_end();
  pthread_join (id, NULL);
  return 0;
}
