// PARAM: --enable ana.int.congruence --set ana.int.refinement fixpoint
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>


int main() {
  int state= rand();

  __goblint_assume(state % 8 == 3);

  int a = state & 0x7f;

  __goblint_check((a== 3));
}
