#include <pthread.h>
#include <goblint.h>

int glob = 5; // not static!

extern void foo(); // unknown function may screw up glob

void *t_fun(void *arg) {
  foo();
  return NULL;
}

int main() {
  pthread_t id;
  __goblint_check(glob == 5);
  pthread_create(&id, NULL, t_fun, NULL);
  __goblint_check(glob == 5); // UNKNOWN!
  return 0;
}
