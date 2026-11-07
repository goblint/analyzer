// SKIP PARAM: --set ana.activated[+] affeq --set sem.int.signed_overflow assume_none
// F passes the address of its local x to its recursive call, so the callee's
// relation keeps the caller activation's formals a, b, c with a == b + 1,
// c == b + 2 and g == a + b. The call rotates the formals. assign_var_parallel
// must keep these relations, renamed to the callee's formals, and forget only
// the callee formals' old values.
#include <goblint.h>
#include <stdlib.h>

int g;

void F(int a, int b, int c, int *p) {
  if (p == NULL) {
    int x = 0;
    g = a + b;
    F(c, a, b, &x);
  } else {
    __goblint_check(a == c + 2);
    __goblint_check(b == c + 1);
    __goblint_check(g == b + c);
    __goblint_check(g == a + b); // FAIL
  }
}

int main(void) {
  int n = rand() % 100;
  F(n + 1, n, n + 2, NULL);
  return 0;
}
