// SKIP PARAM: --sets ana.activated[+] octApron
#include <assert.h>

void main() {
  int x, y, z;

  assert(x < y); // UNKNOWN! (refines)
  assert(y < z); // UNKNOWN! (refines)

  assert(3 <= x); // UNKNOWN! (refines)
  assert(z <= 5); // UNKNOWN! (refines)

  assert(x == 3);
  assert(y == 4);
  assert(z == 5);
}
