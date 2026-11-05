// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval
#include <goblint.h>

int main() {
  int x = 0;
  int *p = &x;
  int y;

  y = *p; // NOWARN
  __goblint_check(y == 0);
  y = *(p + 0); // NOWARN
  __goblint_check(y == 0);
  y = *(p + *p); // NOWARN
  __goblint_check(y == 0);
  y = *(p + p[0]); // NOWARN
  __goblint_check(y == 0);
  y = *(p + p[-1]); // WARN!
  __goblint_check(y == 0); // UNKNOWN!
  y = *(p + 0 * p[-1]); // TODO WARN! (must disable global constant folding)
  __goblint_check(y == 0);
  y = *(p + x * p[-1]); // WARN!
  __goblint_check(y == 0);

  return 0;
}
