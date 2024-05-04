// SKIP PARAM: --set ana.activated[+] apron --set ana.apron.domain polyhedra --set sem.int.signed_overflow assume_none --enable ana.widen.lookahead
// From "Lookahead Widening", Fig. 1: https://doi.org/10.1007/11817963_41
// Checks that also require lookahead, but no narrowing
#include <goblint.h>

int main() {
  int x, y;
  x = 0;
  y = 0;
  while (1) {
    // n_1
    __goblint_check(x >= 0); // needs lookahead
    __goblint_check(x <= 102); // needs lookahead
    __goblint_check(y >= 0); // needs lookahead
    __goblint_check(y <= 51); // needs lookahead
    __goblint_check(y <= x);
    __goblint_check(x + y <= 102); // needs lookahead
    if (x <= 50)
      y++;
    else
      y--;
    if (y < 0)
      break;
    x++;
    // n_6
    __goblint_check(x >= 1); // needs lookahead
    __goblint_check(x <= 102); // needs lookahead
    __goblint_check(y >= 0);
    __goblint_check(y <= 51); // needs lookahead
    __goblint_check(y <= x);
    __goblint_check(x + y <= 102); // needs lookahead
    __goblint_check(51 * y >= 51 - 2 * (x - 1)); // needs lookahead
  }
  // n_x
  __goblint_check(x == 102); // TODO (needs lookahead)
  __goblint_check(y == -1); // needs lookahead
  return 0;
}
