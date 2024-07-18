// SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none 
#include <stdio.h>

int main() {
  int x, y, z;

  z = x;

  __goblint_check(z == x); // SUCCESS

  x = y * y;

  __goblint_check(x == z); // UNKNOWN!

  return 0;
}