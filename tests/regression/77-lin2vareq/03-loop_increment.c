// SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none
#include <stdio.h>

int main() {
  int i, j, k;
  int size = 5;
  i = 0;
  j = 0;
  k = 5;

  for (i = 1; i < size; ++i) {
    j = i;
    k = j + 5;
  }

  __goblint_check(j + 1 == i); // SUCCESS

  __goblint_check(k == i + 4); // SUCCESS

  return 0;
}
