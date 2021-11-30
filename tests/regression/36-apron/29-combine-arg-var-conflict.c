// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
extern int __VERIFIER_nondet_int();

#include <assert.h>

int f(int x) {
  return x + 1;
}

int g(int x) {
  int y = __VERIFIER_nondet_int(); //rand
  if (x < 1000) { // avoid overflow
    y = f(x);
    assert(y == x + 1);
  }
  return x;
}

int main(void) {
  int z = __VERIFIER_nondet_int(); //rand
  int w = __VERIFIER_nondet_int(); //rand
  w = g(z);
  assert(z == w);
  return 0;
}
