#include <pthread.h>

pthread_t id;

extern void magic();

void *t_fun(void *arg) {
  magic(); // invalidates
  return NULL;
}

void main() {
  pthread_create(&id, NULL, t_fun, NULL);
  // mine-W didn't propagate id properly so invalidation set to top of "wrong" type
}
