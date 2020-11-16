#include <pthread.h>
#include <assert.h>

int myglobal = 5;

// atomic by function name prefix
void __VERIFIER_atomic_fun() {
  assert(myglobal == 5);
  myglobal++;
  assert(myglobal == 6);
  myglobal--;
  assert(myglobal == 5);
}

void *t_fun(void *arg) {
  __VERIFIER_atomic_fun();
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  // __VERIFIER_atomic_begin();
  assert(myglobal == 5);
  // __VERIFIER_atomic_end();
  pthread_join (id, NULL);
  return 0;
}
