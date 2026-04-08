// PARAM: --set ana.context.gas_value 1
// Like 00/35 but with gas this time!
// Misbehaves for gas <= 3
#include <goblint.h>
#include <pthread.h>

int g = 1;

void foo() {
  // Single-threaded: g = 1 in local state
  // Multi-threaded: g = 2 in global unprotected invariant
  // Joined contexts: g is unprotected, so read g = 2 from global unprotected invariant (only)
  // Was soundly claiming that check will succeed!
  int x = g;
  __goblint_check(x == 2); // UNKNOWN!
}

void *t_fun(void *arg) {
  foo();
}

int do_stuff() {
  foo();
  g = 2;

  pthread_t id;
  pthread_create(&id, NULL, t_fun, NULL);

  return 0;
}

int main() {
  do_stuff();
  return 0;
}
