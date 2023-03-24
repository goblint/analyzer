// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
extern int __VERIFIER_nondet_int();

#include <goblint.h>

int f(int x) {
  return x + 1;
}

int main(void) {
  int y = __VERIFIER_nondet_int(); //rand
  int z = __VERIFIER_nondet_int(); //rand
  if (y < 1000) { // avoid overflow
    z = y;
    y = f(y);

    // OLD:
    // local is: y == z
    // fun is: #ret == x' + 1
    // fun args subst (x' -> y) is: #ret == y + 1
    // local forget y is: top
    // fun forget y is: top
    // fun subst (#ret -> y) is: top
    // unify is: top

    // WANT/NEW:
    // local is: y == z
    // fun is: #ret == x' + 1
    // fun args subst (x' -> y) is: #ret == y + 1
    // unify is: y == z && #ret == y + 1 (&& #ret == z + 1)
    // assign (y = #ret) is: (internally)
      // 1. y == z && #ret == y + 1 && y#new == #ret (&& #ret == z + 1 && y#new == z + 1)
      // 2. y#new == #ret && #ret == z + 1 (&& y#new == z + 1)
      // 3. y == #ret && #ret == z + 1 (&& y == z + 1)
    // forget #ret is: y == z + 1

    __goblint_check(y == z + 1);
  }
  return 0;
}
