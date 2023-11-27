// PARAM: --enable ana.int.interval
#include <goblint.h>

int main() {
  unsigned long x;
  x = 0;

  int b;
  b = x == 7; // NOWARN

  if (b)
    __goblint_check(0); // NOWARN (unreachable)
  else
    __goblint_check(1); // reachable

  return 0;
}