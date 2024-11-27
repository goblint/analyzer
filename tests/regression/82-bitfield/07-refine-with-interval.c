// PARAM: --enable ana.int.interval --disable ana.int.def_exc --enable ana.int.bitfield --set ana.int.refinement fixpoint
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int main() {
  int a = rand();

  if (a <= 4) {
    __goblint_assert((a & 0x10) == 0); // SUCCESS
  }
}
