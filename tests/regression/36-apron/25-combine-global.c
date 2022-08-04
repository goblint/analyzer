// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
extern int __VERIFIER_nondet_int();

#include <assert.h>

int g;
int h;

void foo() {
  int r = __VERIFIER_nondet_int(); //rand

  if (r) {
    g = 5;
    h = 3;
  }
  else {
    g = 6;
    h = 4;
  }

  __goblint_check(h < g);
  __goblint_check(g - h == 2);
  // return state should contain globals
}

int main(void) {
  int r = __VERIFIER_nondet_int(); //rand

  if (r) {
    g = 3;
    h = 5;
  }
  else {
    g = 4;
    h = 6;
  }

  __goblint_check(g < h);
  __goblint_check(h - g == 2);

  foo(); // combine should use globals from function, not go to bottom due to contradiction with local

  __goblint_check(h < g);
  __goblint_check(g - h == 2);
  return 0;
}
