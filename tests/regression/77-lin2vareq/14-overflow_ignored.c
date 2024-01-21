// SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none

#include <limits.h>
#include <stdio.h>

int main() {
  int x;
  int k;
  int y;

  if (k > INT_MAX - 8) {
    printf("Potential overflow detected.\n");
    return -1;
  }

  x = k + 1;
  __goblint_check(x == k + 1); // SUCCESS

  for (int i = 0; i < 7; i++) {
    x++;
    k++;
  }

  __goblint_check(x == k + 1); // SUCCESS

  return 0;
}
