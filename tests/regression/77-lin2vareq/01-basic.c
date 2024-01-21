//SKIP PARAM: --set ana.activated[+] lin2vareq --set ana.int.enums false --set ana.int.interval false --set ana.int.interval_set false --set ana.int.congruence false --set sem.int.signed_overflow assume_none

#include <stdio.h>
#include <goblint.h>

int main() {
  int x = 0;
  int y = 0;

  x = 1;
  y = 1;

  __goblint_check(x == y); //SUCCESS

  x = 10;

  __goblint_check(x != y); //SUCCESS
  __goblint_check(x == y); //FAIL

  return 0;
}
