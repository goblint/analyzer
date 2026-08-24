// PARAM: --set ana.context.gas_value 4
// Like 00/35 but with gas this time!
// Misbehaves for gas <= 3
#include <goblint.h>
#include <pthread.h>

int g = 1;

void foo() {
  // Check that we don't lose precision due to JoinCall
  int x = g;
  int x2 = g;
  // A hack: In both contexts g has a single possible value, so we check that x = x2
  // to verify there is no precision loss

  __goblint_check(x == x2);
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
