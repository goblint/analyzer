// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --enable ana.int.interval --enable ana.apron.context
extern int __VERIFIER_nondet_int();

#include <assert.h>

int oct(int x, int y) __attribute__((goblint_context("apron.no-context"))); // attributes are not permitted in a function definition
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
    assert(res == 1); // UNKNOWN (indended by no-context attribute)
  }

  res = oct(x, y);
}
