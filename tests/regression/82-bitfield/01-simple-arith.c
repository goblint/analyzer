// PARAM: --disable ana.int.interval --disable ana.int.def_exc --enable ana.int.bitfield
#include <stdlib.h>

int main() {
  int a = 19;
  int b = 23;

  __goblint_check(a + b == 42);
  __goblint_check(a - b == -4);
  __goblint_check(a * b == 437);
  __goblint_check(a / b == 0);
  __goblint_check(a % b == 19);
}