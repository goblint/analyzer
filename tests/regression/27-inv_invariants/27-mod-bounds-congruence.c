// PARAM: --enable ana.int.interval --enable ana.int.congruence
// The interval refinement for "a % b == c" also constrains the congruence domain, so it
// must not meet with a value that is incongruent to c modulo b: that made the branch below
// bot with interval and congruence enabled together, while either domain alone got it right.
#include <goblint.h>

int main() {
  int t;
  if (t >= 0 && t <= 48 && t % 3 == 1) // e.g. t = 4
    __goblint_check(1); // reachable

  int p;
  if (p >= 0 && p <= 24) {
    int u = 2 * p;
    if (u % 3 == 1) // e.g. p = 2 gives u = 4 and 4 % 3 == 1
      __goblint_check(1); // reachable
  }

  return 0;
}
