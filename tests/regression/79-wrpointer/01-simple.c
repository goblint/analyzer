// PARAM: --set ana.activated[+] wrpointer
#include <goblint.h>

void main(void) {
  int *i;
  int **j;
  int *k;
  i = *(j + 3);
  *j = k;

  __goblint_check(**j == *k);
  // j was not initialized, so it may by chance point to &i
  __goblint_check(i == *(j + 3)); // UNKNOWN!

  j = &k + 1;

  __goblint_check(j == &k); // FAIL
}
