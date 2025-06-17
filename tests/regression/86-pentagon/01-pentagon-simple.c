//SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none

// Adapted example from https://link.springer.com/content/pdf/10.1007/BF00268497.pdf

#include <goblint.h>

void main(void) {
  int x = 4;
  __goblint_check(x == 4);
}
