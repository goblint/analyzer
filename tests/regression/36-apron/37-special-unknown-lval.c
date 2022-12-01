// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
extern int __VERIFIER_nondet_int();

#include <goblint.h>
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

  __goblint_check(x == y);

  x = rand(); // invalidates x, LibraryFunction doesn't invalidate globals
  __goblint_check(x == y); // UNKNOWN!
  __goblint_check(y == g);

  g = rand(); // invalidates g
  __goblint_check(y == g); // UNKNOWN!
  __goblint_check(y == r);

  y = magic(); // invalidates y
  __goblint_check(y == r); // UNKNOWN!
}
