#include <pthread.h>
#include <assert.h>

int glob = 5; // not static!

extern void foo(); // unknown function may screw up glob

void *t_fun(void *arg) {
  foo();
  return NULL;
}

int main() {
  pthread_t id;
  assert(glob == 5);
  pthread_create(&id, NULL, t_fun, NULL);
  assert(glob == 5); // UNKNOWN!
  return 0;
}
