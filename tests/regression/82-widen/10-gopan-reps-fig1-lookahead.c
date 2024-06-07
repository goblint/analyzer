// SKIP PARAM: --set ana.activated[+] apron --set ana.apron.domain polyhedra --enable ana.widen.lookahead
// From "Lookahead Widening", Fig. 1: https://doi.org/10.1007/11817963_41
#include <goblint.h>

int main() {
  int x, y;
  x = 0;
  y = 0;
  while (1) {
    __goblint_check(x >= 0); // TODO
    __goblint_check(x <= 102); // TODO
    __goblint_check(y >= 0);
    __goblint_check(y <= 51); // TODO
    if (x <= 50)
      y++;
    else
      y--;
    if (y < 0)
      break;
    x++;
  }
  __goblint_check(x == 102); // TODO
  __goblint_check(y == -1); // TODO
  return 0;
}
