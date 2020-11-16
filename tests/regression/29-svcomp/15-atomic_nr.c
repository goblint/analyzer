#include <pthread.h>

extern void __VERIFIER_atomic_begin();
extern void __VERIFIER_atomic_end();

int myglobal;

void *t_fun(void *arg) {
  __VERIFIER_atomic_begin();
  myglobal=myglobal+1; // NORACE
  __VERIFIER_atomic_end();
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  __VERIFIER_atomic_begin();
  myglobal=myglobal+1; // NORACE
  __VERIFIER_atomic_end();
  pthread_join (id, NULL);
  return 0;
}
