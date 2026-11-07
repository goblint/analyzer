// SKIP PARAM: --set ana.activated[+] affeq --disable ana.affeq.sparse --enable ana.int.interval --set sem.int.signed_overflow assume_none
// As 24-recursion-constrained-formal-relation.c, with the dense affeq
// implementation, which has the same assign_var_parallel.
#include <goblint.h>
#include <stdlib.h>

int g;

void F(int a, int *p) {
  if (p == NULL) {
    int x = 0;
    g = a - 1;
    F(10, &x);
  } else {
    __goblint_check(g == -1); // UNKNOWN!
    __goblint_check(g >= -1);
  }
}

int main(void) {
  F(rand() % 100, NULL);
  return 0;
}
