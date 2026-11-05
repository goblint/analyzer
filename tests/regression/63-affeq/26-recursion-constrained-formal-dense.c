// SKIP PARAM: --set ana.activated[+] affeq --disable ana.affeq.sparse --set sem.int.signed_overflow assume_none
// As 22-recursion-constrained-formal.c, with the dense affeq implementation,
// which has the same assign_var_parallel.
#include <goblint.h>
#include <stdlib.h>

void F(int a, int b, int *p) {
  int y = 0;
  int w = rand() % 100;
  if (p == NULL) {
    int x = 0;
    F(w, w + 1, &x);
  } else {
    *p = 1;
    __goblint_check(b == a + 1);
  }
}

int main(void) {
  F(0, 5, NULL);
  return 0;
}
