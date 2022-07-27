#include <assert.h>

void proc(int *x, int *y) {}

main () {
  int z = 1;
  proc(&z, &z);
  assert(z == 1);
}
