// SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none --set ana.int.def_exc false --set ana.int.enums false --set ana.int.interval false --set ana.int.interval_set false --set ana.int.congruence false


#include <stdio.h>

int main() {
  int x, y, z;

  z = x;

  __goblint_check(z == x); // SUCCESS

  x = y * y;

  __goblint_check(x == z); // UNKNOWN!

  return 0;
}