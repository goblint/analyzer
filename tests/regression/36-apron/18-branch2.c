// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
// Based on 36/09.
extern int __VERIFIER_nondet_int();

#include <goblint.h>

void main() {
  int i = __VERIFIER_nondet_int(); //rand
  if (i) { // same as i != 0
    // only implies i != 0
    // doesn't imply i > 0
    // doesn't imply i >= 1
    __goblint_check(i >= 1); // UNKNOWN!
  }
  else {
    // implies i == 0
    // doesn't imply i < 0
    __goblint_check(i == 0);
    __goblint_check(i < 0); // FAIL
  }
}
