// PARAM: --enable ana.int.interval --enable ana.int.bitfield --set ana.int.refinement fixpoint --trace inv --trace branch --trace invariant
#include <goblint.h>

int main() {
  unsigned char r; // non-neg rand
  char x = r % 64;

  if ((x | 0) == 63) {
    __goblint_check(x == 63); // SUCCESS
  }

  if ((x & 63) == 0) {
    __goblint_check(x == 0); // SUCCESS
  }


}
