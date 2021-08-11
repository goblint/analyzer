// SKIP PARAM: --sets ana.activated[+] octApron
#include <assert.h>
#include <stdlib.h>

extern int magic();

int g;

void main() {
  int r, x, y;

  x = r;
  y = r;
  g = r;

  assert(x == y);

  x = rand(); // invalidates x, LibraryFunction doesn't invalidate globals
  assert(x == y); // UNKNOWN!
  assert(y == g);

  g = rand(); // invalidates g
  assert(y == g); // UNKNOWN!
  assert(y == r);

  y = magic(); // invalidates y
  assert(y == r); // UNKNOWN!
}
