// SKIP PARAM: --sets ana.activated[+] octApron
#include <assert.h>

int g;
int h;

int f(int x) {
  return x - 2;
}

int main(void) {
  int r;

  g = f(r);
  h = r;

  assert(g < h);
  assert(h - g == 2);
  return 0;
}
