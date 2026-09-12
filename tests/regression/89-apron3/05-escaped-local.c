// PARAM: --set ana.activated[+] apron --enable ana.sv-comp.functions
#include <goblint.h>

static int *target;

static void modify(void) {
  *target = __VERIFIER_nondet_int();
}

int main(void) {
  int left = __VERIFIER_nondet_int();
  int right = left;
  target = &right;

  modify();
  __goblint_check(left == right); // UNKNOWN!
}
