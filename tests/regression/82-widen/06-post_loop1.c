// PARAM: --set solvers.td3.widen_gas 6 --enable ana.int.interval
#include <goblint.h>

int main(void) {
  int a;
  int b = 0;

  for(a = 0; a < 5; a ++) {
    b = b < a ? a : b;
    // widening gas cannot help here:
    // b += a;
    // even though the interval of a eventually stabilizes at [0, 4],
    // we are not tracking the number of possible iterations.
    // Hence, the interval of b keeps growing, as it can be
    // increased by 4 each iteration.
  }
  __goblint_check(b < 5);

  return 0;
}
