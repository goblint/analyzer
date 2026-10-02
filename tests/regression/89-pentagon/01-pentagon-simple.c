// SKIP PARAM: --conf ./conf/pentagon.json

#include <goblint.h>

void main(void) {
  int x = rand(), y = x, z = x;
  y = x - 1;
  z = y - 1;
  __goblint_check(z < x); // SUCC
}
