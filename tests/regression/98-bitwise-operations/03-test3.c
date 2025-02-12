// PARAM: --enable ana.int.def_exc
#include <goblint.h>

int main() {
  unsigned int x;

  unsigned int y = 2;

  unsigned int z = x & y;

  __goblint_check(z <= 2); // TODO

  return 0;
}