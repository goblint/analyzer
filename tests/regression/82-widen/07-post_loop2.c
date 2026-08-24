// PARAM: --set solvers.td3.widen_gas 5 --enable ana.int.interval --enable exp.no-narrow
#include <goblint.h>

int main(void) {
  // represents non-deterministic value
  int unknown;
  int a = unknown > 9 ? 9 : (unknown < -9 ? -9 : unknown);

  while (-10 < a && a < 10) {
    a = -2 * (a - 1);
  }

  __goblint_check(-16 <= a);
  __goblint_check(a <= 20);

  return 0;
}
