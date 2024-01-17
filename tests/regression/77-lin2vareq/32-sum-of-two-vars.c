// SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none

#include <stdio.h>

int main() {
  int x, y, z, w, k;

  z = y;
  x = y;
  w = y;

  __goblint_check(z == 2 * x - y); // SUCCESS

  k = z + w - x + 5;

  __goblint_check(k == y + 5); //SUCCESS

    y = 3;
  __goblint_check(k == y + 5); // UNKNOWN!


  return 0;
}
