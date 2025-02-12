// PARAM: --enable ana.int.interval
#include <goblint.h>

int main() {
  unsigned int x;

  unsigned int y = x & 2;

  unsigned int z = y & 4;

  __goblint_check(z <= 2); // TODO

  int a;

  if (a >= 0) {
    a = -1;
  }

  int b = a & 2;

  __goblint_check(b >= 0); // TODO

  return 0;
}