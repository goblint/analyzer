// SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none

#include <stdio.h>

int main() {
  int x;
  int k;
  int y;

  x = k + 1;

  __goblint_check(x == k + 1); // SUCCESS


  return 0;
}
