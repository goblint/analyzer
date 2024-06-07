// SKIP PARAM: --set ana.activated[+] apron --set ana.apron.domain polyhedra --set sem.int.signed_overflow assume_none --enable ana.widen.lookahead
// From "Lookahead Widening", Fig. 1: https://doi.org/10.1007/11817963_41
#include <goblint.h>

int main() {
  int x, y;
  x = 0;
  y = 0;
  while (1) {
    __goblint_check(x >= 0); // TODO
    __goblint_check(x <= 102); // TODO
    __goblint_check(y >= 0); // TODO (with assume_none)
    __goblint_check(y <= 51); // TODO
    __goblint_check(y <= x); // TODO (without assume_none?)
    __goblint_check(x + y <= 102); // TODO
    if (x <= 50)
      y++;
    else
      y--;
    if (y < 0)
      break;
    x++;
  }
  __goblint_check(x == 102); // TODO (needs narrowing?)
  __goblint_check(y == -1); // TODO
  return 0;
}
