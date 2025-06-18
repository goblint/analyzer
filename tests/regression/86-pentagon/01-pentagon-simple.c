//SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none

// Adapted example from https://link.springer.com/content/pdf/10.1007/BF00268497.pdf

// Run with ./goblint tests/regression/86-pentagon/01-pentagon-simple.c --conf ./conf/empty.json

#include <goblint.h>

void main(void) {
  int x = 24;
  int y = 1;
  int z = 23;

  __goblint_check(x - y >= z - y);
}
