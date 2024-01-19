// SKIP PARAM: --set ana.activated[+] lin2vareq

#include <stdio.h>
#include <limits.h>

int main() {
  int x;
  int k;
  int y;

  x = k + 1;

  __goblint_check(x == k + 1); // UNKNOWN
  
  for (int i = 0; i < 7; i++) {
     if (x > INT_MAX - 1 || k > INT_MAX - 1) {
      printf("Potential overflow detected.\n");
      return -1;
    }
    x++;
    k++;
  }

  __goblint_check(x == k + 1); // UNKNOWN
  return 0;
}