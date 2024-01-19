//SKIP PARAM: --set ana.activated[+] lin2vareq

#include <stdio.h>
#include <limits.h>

int main() {
  int x;
  int k;
  int y = 5;

  if (x > INT_MAX / 3 || k > INT_MAX / 2 || k > INT_MAX / x) {
    printf("Potential overflow detected.\n");
    return -1;
  }

  int result1 = 3 * (x + y) - 2 * x + 6;
  int result2 = 3 * (x + y) - 2 * k + 6;
  int result3 = x * 3 - x * 2;
  int result4 = x * 3 - x * k * x;

  __goblint_check(result1 == x + 21); // UNKNOWN!
  __goblint_check(result2 == x + 21); // UNKNOWN!
  __goblint_check(result3 == x);      // UNKNOWN!
  __goblint_check(result4 == x * 3 - x * k * x); // UNKNOWN!

  return 0;
}


