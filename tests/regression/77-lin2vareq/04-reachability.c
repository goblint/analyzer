// SKIP PARAM: --set ana.activated[+] lin2vareq --set ana.int.enums false --set ana.int.interval false --set ana.int.interval_set false --set ana.int.congruence false
#include <stdio.h>
#include <goblint.h>

int main() {
  int x = 0;
  int y = 0;

  x = 10;
  y = 1;

  __goblint_check(x == 10 * y); //SUCCESS

  if(x == 10 * y)
    return 0;
  __goblint_check(0); // NOWARN (unreachable)
}
