// SKIP PARAM: --set ana.activated[+] affeq --enable ana.int.interval --set sem.int.signed_overflow assume_none
// The outer activation F(n, NULL) sets g = a - 1 and passes the address of its
// local x to the recursive call, so the callee's relation keeps a == g + 1.
// body then assigns the formal a from its argument. assign_var_parallel must
// drop the old value's constraint: overwriting the column of a with the column
// of the new value turns a - g - 1 = 0 into -g - 1 = 0, and the analysis would
// claim g == -1, which fails whenever rand() % 100 is not 0.
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
