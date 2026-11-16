// PARAM: --enable ana.sv-comp.functions
#include <stdlib.h>
#include <goblint.h>

extern int __VERIFIER_nondet_int(void);

int main(void) {
  int *p = malloc(2 * sizeof(int));
  int *q = __VERIFIER_nondet_int() ? p : p + 1;

  __goblint_check(p == q); // UNKNOWN!
  if (p != q) // Above check used to be unknown, but this true branch was unsoundly dead (issue #2319).
    __goblint_check(1); // reachable
  return 0;
}
