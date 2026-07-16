// PARAM: --enable ana.int.interval --disable ana.int.def_exc --enable ana.int.bitfield --set ana.int.refinement fixpoint
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int main() {
  int a = rand();

  if (a <= 4) {
    __goblint_assert((a & 0x10) == 0); // SUCCESS

    int b = ~0x7;
    __goblint_assert((a & b) == 0);  // SUCCESS
  }

  if (a > 8 && a < 15) {
    __goblint_assert((a & 8) == 8);  // SUCCESS
  }

  int b = rand() - 512;

  if(-4 <= b && b <= -2) {
    __goblint_assert((b & 4) == 4);  // SUCCESS
  }
}
