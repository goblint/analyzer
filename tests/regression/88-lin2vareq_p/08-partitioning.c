// SKIP PARAM: --set ana.activated[+] lin2vareq_p  --set sem.int.signed_overflow assume_none
// example from https://dl.acm.org/doi/10.1145/2049706.2049710

#include <goblint.h>
#include <limits.h>
#include <stdio.h>

int main() {

  int x, x1, x2, x3, x4, x5, x6, x7;

  if (x1 > INT_MAX - 5 || x2 > INT_MAX - 5 || x2 > INT_MIN + 5) {
    return -1;
  }

  if (x > 5) {
    x1 = x1;
    x2 = x2;
    x3 = x1;
    x4 = x2 + 5;
    x5 = x1 + 5;
    x6 = x1 + 3;
    x7 = x1 + 2;
  } else {
    x1 = x1;
    x2 = x2;
    x3 = x2 - 5;
    x4 = x2 + 5;
    x5 = x2;
    x6 = x2 + 1;
    x7 = x2;
  }

  __goblint_check(x4 == x2 + 5); // SUCCESS
  __goblint_check(x5 == x3 + 5); // SUCCESS
  __goblint_check(x7 == x6 - 1); // SUCCESS

  return 0;
}
