// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
#include <assert.h>

extern void magic();

int g;
int h;

void main() {
  int r, x;
  if (r < 1000) { // avoid overflow
    x = r;
    g = r;
    h = r + 1;

    assert(g < h);
    magic(); // invalidates (forgets) globals
    assert(g < h); // UNKNOWN!
    assert(x == r); // shouldn't forget locals
  }
}
