extern int __VERIFIER_nondet_int();

// SKIP PARAM: --set ana.activated[+] apron
#include <assert.h>

int g;
int h;

void f(int x, int y) {
  assert(x < y);
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
