// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
extern int __VERIFIER_nondet_int();

#include <assert.h>

void main() {
  int x = __VERIFIER_nondet_int();
  int a = 0;
  if (x > -1000 && x < 1000) { // avoid under-/overflow
    if (2 * x == 3) { // apron excludes true branch
      a = 1;
    }
    assert(a == 0);
  }
}
