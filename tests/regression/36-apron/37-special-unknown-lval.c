// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
extern int __VERIFIER_nondet_int();

#include <assert.h>
#include <stdlib.h>

extern int magic();

int g;

void main() {
  int r = __VERIFIER_nondet_int();
  int x = __VERIFIER_nondet_int();
  int y = __VERIFIER_nondet_int();

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
