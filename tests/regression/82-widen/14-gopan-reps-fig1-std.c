// SKIP PARAM: --set ana.activated[+] apron --set ana.apron.domain polyhedra --set sem.int.signed_overflow assume_none
// From "Lookahead Widening", Fig. 1: https://doi.org/10.1007/11817963_41
// Checks that require no narrowing or lookahead
#include <goblint.h>

int main() {
  int x, y;
  x = 0;
  y = 0;
  while (1) {
    // n_1
    __goblint_check(y <= x);
    if (x <= 50)
      y++;
    else
      y--;
    if (y < 0)
      break;
    x++;
    // n_6
    __goblint_check(x >= 0);
    __goblint_check(y >= 0);
    __goblint_check(y <= x);
  }
  // n_x
  __goblint_check(y <= -1);
  __goblint_check(x >= y - 1);
  return 0;
}
