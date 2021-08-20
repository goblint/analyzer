// SKIP PARAM: --sets ana.activated[+] octApron
#include <assert.h>

int g;

int f() {
  return g;
}

int main(void) {
  int x, y;
  g = x;
  y = f();
  assert(y == g);
  assert(x == g); // TODO (only when singlethreaded)
  assert(x == y); // TODO (only when singlethreaded)
  return 0;
}
