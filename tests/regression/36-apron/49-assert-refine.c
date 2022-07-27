// SKIP PARAM: --sets ana.activated[+] apron
#include <assert.h>

void main() {
  int x, y, z;

  // TODO: make these asserts after distinction
  __goblint_commit(x < y); // U NKNOWN! (refines)
  __goblint_commit(y < z); // U NKNOWN! (refines)

  __goblint_commit(3 <= x); // U NKNOWN! (refines)
  __goblint_commit(z <= 5); // U NKNOWN! (refines)

  assert(x == 3);
  assert(y == 4);
  assert(z == 5);
}
