#include <assert.h>

void proc(int *x, int *y) {}

main () {
  int z = 1;
  proc(&z, &z);
  __goblint_check(z == 1);
}
