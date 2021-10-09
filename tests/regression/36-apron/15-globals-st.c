extern int __VERIFIER_nondet_int();

// SKIP PARAM: --set ana.activated[+] apron --disable ana.int.interval
#include <assert.h>

int g = 0;

int main(void) {
  int x = __VERIFIER_nondet_int(); // rand
  int y = __VERIFIER_nondet_int(); //rand
  int r = __VERIFIER_nondet_int(); //rand

  if (r) {
    g = 1;
  }

  // using apron interval
  assert(0 <= g);
  assert(g <= 1);

  g = r;

  x = g;
  y = g;
  assert(x == y);
  return 0;
}
