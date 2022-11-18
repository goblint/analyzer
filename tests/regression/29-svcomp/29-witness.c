// PARAM: --enable ana.sv-comp.enabled --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )"
#include <pthread.h>
#include <goblint.h>

// Global is never accessed, hence information about protecting mutexes is implicit \bot
int g1 = 5;

void *t_fun(void *arg) {
  return NULL;
}

int main(void) {
  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);
  // We try to write out witness information here, including the deref of ptr
  // Thus, we also need to make the implicit assumption that things that are missing in the hash table are \bot
  int* ptr = &g1;
  return 0;
}
