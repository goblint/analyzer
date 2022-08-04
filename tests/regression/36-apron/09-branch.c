// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
// Based on 01/35.
extern int __VERIFIER_nondet_int();

#include <assert.h>

void main() {
  int i = __VERIFIER_nondet_int();
  if (i - 1) {
    // only implies i - 1 != 0    (i != 1)
    // doesn't imply i - 1 == 1   (i == 2)
    // doesn't imply i - 1 != 1   (i != 2)
    __goblint_check(i == 2); // UNKNOWN!
  }

  two();
}

void two() {
  int i = __VERIFIER_nondet_int();
  int j =  __VERIFIER_nondet_int();

  if(i<j) {
    __goblint_check(i<j);
  }

  if(!(i<j)) {

  } else {
    __goblint_check(i<j);
  }
}
