// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --disable ana.int.interval
extern int __VERIFIER_nondet_int();

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
  __goblint_check(0 <= g);
  __goblint_check(g <= 1);

  g = r;

  x = g;
  y = g;
  __goblint_check(x == y);
  return 0;
}
