#include <pthread.h>

int myglobal;

// atomic by function name prefix
void __VERIFIER_atomic_fun() {
  myglobal=myglobal+1; // NORACE
}

void *t_fun(void *arg) {
  __VERIFIER_atomic_fun();
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  __VERIFIER_atomic_fun();
  pthread_join (id, NULL);
  return 0;
}
