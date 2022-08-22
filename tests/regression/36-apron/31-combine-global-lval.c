// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
extern int __VERIFIER_nondet_int();

#include <assert.h>

int g;
int h;

int f(int x) {
  return x - 2;
}

int main(void) {
  int r = __VERIFIER_nondet_int(); //rand
  if (r > -1000) { // avoid underflow
    g = f(r);
    h = r;

    __goblint_check(g < h);
    __goblint_check(h - g == 2);
  }
  return 0;
}
