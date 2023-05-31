// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --enable ana.int.interval --disable ana.relation.context
extern int __VERIFIER_nondet_int();

#include <goblint.h>

int oct(int x, int y) __attribute__((goblint_context("relation.context"))); // attributes are not permitted in a function definition
int oct(int x, int y) {
  int s;
  if (x <= y)
    s = 1;
  else
    s = 0;
  return s;
}

void main() {
  int x = __VERIFIER_nondet_int(); //rand
  int y = __VERIFIER_nondet_int(); //rand
  int res;
  if (x <= y) {
    res = oct(x, y);
    __goblint_check(res == 1);
  }

  res = oct(x, y);
}
