// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
extern int __VERIFIER_nondet_int();

#include <assert.h>

int g;

int f() {
  return g;
}

int main(void) {
  int x = __VERIFIER_nondet_int(); //rand
  int y = __VERIFIER_nondet_int(); //rand
  g = x;
  y = f();
  assert(y == g);
  assert(x == g); // TODO (only when singlethreaded)
  assert(x == y); // TODO (only when singlethreaded)
  return 0;
}
