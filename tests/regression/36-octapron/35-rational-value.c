// SKIP PARAM: --sets ana.activated[+] octApron
#include <assert.h>

void main() {
  int x, a = 0;
  if (2 * x == 3) { // apron excludes true branch
    a = 1;
  }
  assert(a == 0);
}
