// PARAM: --enable ana.sv-comp.functions
#include <pthread.h>
#include <assert.h>

extern void __VERIFIER_atomic_begin();
extern void __VERIFIER_atomic_end();

int myglobal = 0;
int myglobal2 = 0;
int myglobal3 = 0;

void *t_fun(void *arg) {
  __VERIFIER_atomic_begin();
  myglobal2++;
  __VERIFIER_atomic_end();
  __VERIFIER_atomic_begin();
  myglobal++;
  __VERIFIER_atomic_end();
  return NULL;
}

void *t2_fun(void *arg) {
  __VERIFIER_atomic_begin();
  myglobal3++;
  __VERIFIER_atomic_end();
  __VERIFIER_atomic_begin();
  myglobal++;
  __VERIFIER_atomic_end();
  return NULL;
}

int main(void) {
  pthread_t id, id2;
  pthread_create(&id, NULL, t_fun, NULL);
  pthread_create(&id2, NULL, t2_fun, NULL);
  assert(myglobal == 2); // UNKNOWN!
  __VERIFIER_atomic_begin();
  assert(myglobal == 2); // UNKNOWN!
  __VERIFIER_atomic_end();
  pthread_join (id, NULL);
  pthread_join (id2, NULL);
  return 0;
}
