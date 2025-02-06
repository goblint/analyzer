// PARAM: --enable ana.int.bitfield --set ana.int.refinement fixpoint
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int main() {
  int a = rand();

  int inv_mask = ~0xe;  // inv_mask = 0b1111.1111.1111.1111.1111.1111.1111.0001 in binary

  if ((a & inv_mask) == 0) {
    // a should get refined 0b0000.0000.0000.0000.0000.0000.0000.⊤⊤⊤0 in binary
    __goblint_check(a <= 14);  // SUCCESS
    __goblint_check(a >= 0);   // SUCCESS

    if (0 <= a && a <= 14) {
      printf("a is in the interval [0, 14]\n");
    } else {
      __goblint_check(0);  // NOWARN (unreachable)
    }
  }
}