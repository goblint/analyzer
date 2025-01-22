// PARAM: --enable ana.int.interval
#include <goblint.h>

int main() {
  unsigned int x;

  unsigned int y = x & 2;

  unsigned int z = y & 4;

  __goblint_check(z <= 2); // TODO 

  return 0;
}