//SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none

// Adapted example from https://link.springer.com/content/pdf/10.1007/BF00268497.pdf

// Run with ./goblint tests/regression/86-pentagon/01-pentagon-simple.c --conf ./conf/empty.json

#include <goblint.h>

void main(void) {
  // Cil convert exception Var_not_found:
  int x;
  __goblint_check(x == 0);

  // Cil convert execption Overflow:
  /*int y = 0;
  __goblint_check(y + 1 > 0);*/

  // works
  /*int z = 0;
  __goblint_check (z == 0);*/
}
