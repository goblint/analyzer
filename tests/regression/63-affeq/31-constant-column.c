// SKIP PARAM: --set ana.activated[+] affeq --set sem.int.signed_overflow assume_none
#include <goblint.h>

// Exercise the constant column through both kinds of assignment and expression evaluation.
void assignments(int n) {
  int x = n + 5;
  __goblint_check(x - n == 5);
  __goblint_check(x - n == -5); // FAIL

  x = x + 3;
  __goblint_check(x - n == 8);

  x = 2 * x - n + 1;
  __goblint_check(x - n == 17);

  x = n - 4;
  __goblint_check(x - n == -4);

  x = -3;
  int y = x + n;
  __goblint_check(y - n == -3);
  __goblint_check(x == -3);
}

// A guard introduces an equality row; assignment must preserve its offset.
void guarded(int x, int n) {
  if (x - n == 5) {
    __goblint_check(x - n == 5);
    x = x + 2;
    __goblint_check(x - n == 7);
  }
}

// Joining branches with different constants must retain their common relation.
void joined(int n, int branch) {
  int x, y;
  if (branch) {
    x = n + 5;
    y = x + 3;
  } else {
    x = n - 4;
    y = x + 3;
  }

  __goblint_check(y - x == 3);
}

int main(void) {
  int n, x, branch;
  assignments(n);
  guarded(x, n);
  joined(n, branch);
  return 0;
}
