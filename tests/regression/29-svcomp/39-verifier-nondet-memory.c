// PARAM: --enable ana.sv-comp.functions
#include <stdlib.h>
#include <goblint.h>

void __VERIFIER_nondet_memory(void *mem, size_t size);

struct structType {
  int f;
  int g;
};

enum A {
  a, b, c
};

int main() {
  int x = 0;
  __VERIFIER_nondet_memory(&x, sizeof(int));
  __goblint_check(x == 0); // UNKNOWN!

  struct structType s;
  s.f = s.g = 0;
  __VERIFIER_nondet_memory(&s, sizeof(s));
  __goblint_check(s.f == 0); // UNKNOWN!
  __goblint_check(s.g == 0); // UNKNOWN!

  int *values = malloc(sizeof(int) * 20);
  values[0] = values[1] = 0;
  __VERIFIER_nondet_memory(values, sizeof(int) * 20);
  __goblint_check(values[0] == 0); // UNKNOWN!
  __goblint_check(values[1] == 0); // UNKNOWN!

  int *p = &x;
  x = 0;
  __VERIFIER_nondet_memory(&p, sizeof(int*));
  __goblint_check(p == &x); // UNKNOWN!
  __goblint_check(x == 0);

  enum A e;
  __VERIFIER_nondet_memory(&e, sizeof(enum A));

  if (e != a && e != b && e != c) {
    __goblint_check(1); // reachable (compiler needs to assign at least 2 bits for type enum A)
  }
  return 0;
}
