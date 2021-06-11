// SKIP PARAM: --sets ana.activated[+] octApron --disable ana.int.def_exc
#include <assert.h>

int g = 0;

int main(void) {
  int x, y, r;

  assert(g == 0);

  g = r;

  x = g;
  y = g;
  assert(x == y);
  return 0;
}
