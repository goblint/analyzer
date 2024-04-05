// PARAM: --set ana.activated[+] wrpointer
#include <goblint.h>

void main(void) {
  int *i;
  int **j;
  int *k;
  i = *(j + 3);
  *j = k;

  __goblint_check(i == *(j + 3));
  __goblint_check(*j == k);
}
