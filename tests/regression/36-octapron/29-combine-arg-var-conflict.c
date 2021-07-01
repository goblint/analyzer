// SKIP PARAM: --sets ana.activated[+] octApron
#include <assert.h>

int f(int x) {
  return x + 1;
}

int g(int x) {
  int y;
  y = f(x);
  assert(y == x + 1);
  return x;
}

int main(void) {
  int z, w;
  w = g(z);
  assert(z == w);
  return 0;
}
