// SKIP PARAM: --sets ana.activated[+] octApron
#include <assert.h>

extern void magic();

int g;
int h;

void main() {
  int r, x;

  x = r;
  g = r;
  h = r + 1;

  assert(g < h);
  magic(); // invalidates (forgets) globals
  assert(g < h); // UNKNOWN!
  assert(x == r); // shouldn't forget locals
}
