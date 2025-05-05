// PARAM: --disable ana.int.def_exc --enable ana.int.bitfield --set ana.int.refinement fixpoint --enable ana.int.enums
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int main() {
  int a = rand();

  if (a == 9 || a == 11 || a == 15) {
    __goblint_assert((a & 9) == 9); // SUCCESS
  }
}
