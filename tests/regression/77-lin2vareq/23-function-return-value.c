// SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none

#include <goblint.h>

int k;

int x_plus_three(int x) {
  return x + 3;
}

int main(void) {
  int y, z;
  z = x_plus_three(k);

  __goblint_check(z == k + 3);

  return 0;
}
