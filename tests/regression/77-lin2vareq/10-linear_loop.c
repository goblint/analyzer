// SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none

#include <goblint.h>
#include <stdio.h>

int main() {
  int x = 1;
  int y = 1;
  int z = 1;
  int k;

  for (int i = 1; i <= 3; i++) {
    x = x * i;
    y = x;
    z = y + (y - x) + 2;
    __goblint_check(x == y); // SUCCESS
    __goblint_check(z == y + 2); // SUCCESS
  }

  return 0;
}
