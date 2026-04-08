// PARAM: --set solvers.td3.widen_gas 5 --enable ana.int.interval
#include <goblint.h>

int main(void) {
  int a;
  int b;

  for(a = 0; a != 3; a ++)
    __goblint_check(a < 3);
  for(b = 0; b != 4; b ++)
    __goblint_check(b < 4);

  return 0;
}
