// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
extern int __VERIFIER_nondet_int();

#include <assert.h>

int g;
int h;

void f(int x, int y) {
  __goblint_check(x < y);
}

int main(void) {
  int r = __VERIFIER_nondet_int(); //rand
  g = r;
  h = r + 1;

  if (g < h) {
    f(g, h);
  }
  return 0;
}
