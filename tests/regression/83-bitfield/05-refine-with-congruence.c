// PARAM: --disable ana.int.interval --disable ana.int.def_exc --enable ana.int.bitfield --set ana.int.refinement fixpoint --enable ana.int.congruence
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <goblint.h>

int main() {
  int a = rand();

  __goblint_assume(a % 8 == 3);

  __goblint_assert((a & 0x7) == 3); // SUCCESS

}

