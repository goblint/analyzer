// PARAM: --set ana.activated[+] wrpointer
#include <goblint.h>

void main(void) {
  long *i;
  long **j;
  long *k;
  j = &k + 1;
  j++;
  __goblint_check(j == &k + 2);

  i = *(j + 3);
  i++;
  __goblint_check(i == *(j + 3) + 1);
  j++;
  __goblint_check(i == *(j + 2) + 1);
}
