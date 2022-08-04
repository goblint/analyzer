// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
extern int __VERIFIER_nondet_int();

#include <assert.h>

extern void magic();

int g;
int h;

void main() {
  int r = __VERIFIER_nondet_int();
  int x = __VERIFIER_nondet_int();
  if (r < 1000) { // avoid overflow
    x = r;
    g = r;
    h = r + 1;

    __goblint_check(g < h);
    magic(); // invalidates (forgets) globals
    __goblint_check(g < h); // UNKNOWN!
    __goblint_check(x == r); // shouldn't forget locals
  }
}
