// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
#include <assert.h>

int g;
int h;

int f(int x) {
  return x - 2;
}

int main(void) {
  int r;
  if (r > -1000) { // avoid underflow
    g = f(r);
    h = r;

    assert(g < h);
    assert(h - g == 2);
  }
  return 0;
}
