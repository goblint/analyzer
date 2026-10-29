// SKIP PARAM: --set ana.activated[+] affeq --disable ana.affeq.sparse --set sem.int.signed_overflow assume_none
// As 26-recursion-rotated-formals.c, with the dense affeq implementation,
// which has the same assign_var_parallel.
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
