// SKIP PARAM: --set ana.activated[+] lin2vareq

#include <stdio.h>

int main() {
  int x;
  int k;
  int y;

  x = k + 1;
  //there might be an overflow
  __goblint_check(x == k + 1); // UNKNOWN!

  int unknown;

  if (unknown < 300 && unknown > 0) {
    x = unknown;
    // an overflow is not possible
    __goblint_check(x == unknown); // SUCCESS
  }

  return 0;
}
