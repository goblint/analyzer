// SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none

#include <stdio.h>

int main() {
  int x;
  int k;
  int y = 5;

  int result1 = 3 * (x + y) - 2 * x + 6;
  int result2 = 3 * (x + y) - 2 * k + 6;
  int result3 = x * 3 - x * 2;
  int result4 = x * 3 - x * k * x;

  __goblint_check(result1 == x + 21); // SUCCESS
  __goblint_check(result2 == x + 21); // UNKNOWN!
  __goblint_check(result3 == x);      // SUCCES
  __goblint_check(result4 == x * 3 - x * k * x); // UNKNOWN!

  return 0;
}

// This test case includes variable with unknown values
