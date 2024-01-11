// SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none

#include <stdio.h>

int main() {
  int x, y, z;

  z = y;
  x = y;

  __goblint_check(z == 2 * x - y); // SUCCESS
  y = 3;
  __goblint_check(z == 2 * x - y); // UNKNOWN

  return 0;
}
