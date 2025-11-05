// PARAM: --disable ana.int.interval --disable ana.int.def_exc --enable ana.int.bitfield
#include <stdlib.h>

int main() {
  int a = 19;
  int b = 14;

  __goblint_check((a & b) == 2);
  __goblint_check((a | b) == 31);
  __goblint_check((a ^ b) == 29);
  __goblint_check((~a) == -20);
  __goblint_check((a << 2) == 76);
  __goblint_check((a >> 2) == 4);
}