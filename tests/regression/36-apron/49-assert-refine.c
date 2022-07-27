// SKIP PARAM: --sets ana.activated[+] apron
#include <assert.h>

void main() {
  int x, y, z;

  // TODO: make these asserts after distinction
  __goblint_commit(x < y); // U NKNOWN! (refines)
  __goblint_commit(y < z); // U NKNOWN! (refines)

  __goblint_commit(3 <= x); // U NKNOWN! (refines)
  __goblint_commit(z <= 5); // U NKNOWN! (refines)

  __goblint_check(x == 3);
  __goblint_check(y == 4);
  __goblint_check(z == 5);
}
