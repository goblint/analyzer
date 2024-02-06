// SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none

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
