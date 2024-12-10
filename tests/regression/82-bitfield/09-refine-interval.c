// PARAM: --enable ana.int.bitfield --set ana.int.refinement fixpoint
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int main() {
  int a = rand();

  // 1110 in binary
  int inv_mask = ~0xe;  // 1111...10001 in binary

  if ((a & inv_mask) == 0) {
    __goblint_check(a <= 14);  // SUCCESS
    __goblint_check(a >= 1);   // SUCCESS

    if (1 <= a && a <= 14) {
      printf("a is in the interval [1, 14]\n");
    } else {
      __goblint_check(0);  // NOWARN (unreachable)
    }
  }
}