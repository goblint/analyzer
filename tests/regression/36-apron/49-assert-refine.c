// SKIP PARAM: --sets ana.activated[+] apron
#include <assert.h>

void main() {
  int x, y, z;

  // TODO: make these asserts after distinction
  __goblint_assume(x < y); // U NKNOWN! (refines)
  __goblint_assume(y < z); // U NKNOWN! (refines)

  __goblint_assume(3 <= x); // U NKNOWN! (refines)
  __goblint_assume(z <= 5); // U NKNOWN! (refines)

  __goblint_check(x == 3);
  __goblint_check(y == 4);
  __goblint_check(z == 5);
}
