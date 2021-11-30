// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
extern int __VERIFIER_nondet_int();

#include <assert.h>

void foo() {

}

int main(void) {
  int x = __VERIFIER_nondet_int(); //rand
  int y = __VERIFIER_nondet_int(); //rand
  int r = __VERIFIER_nondet_int(); //rand

  if (r) {
    x = 3;
    y = 5;
  }
  else {
    x = 4;
    y = 6;
  }

  assert(x < y);
  assert(y - x == 2);

  foo(); // combine without lval shouldn't ruin local state

  assert(x < y);
  assert(y - x == 2);
  return 0;
}
