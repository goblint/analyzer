// SKIP PARAM: --set ana.activated[+] affeq --set sem.int.signed_overflow assume_none
// This test was added from 77-lin2vareq because it found mistakes in dim_add that weren't detected by the other tests
#include <goblint.h>

int check_equal(int x, int y, int z) {
  __goblint_check(x == y);
  __goblint_check(z == y);
  __goblint_check(x == z);
  return 8;
}

int main(void) {
  int x, y, z;

  y = x;
  z = y;

  check_equal(x, y, z);

  return 0;
}
