// PARAM: --set ana.activated[+] wrpointer
#include <goblint.h>
#include <stdlib.h>

void main(void) {
  int *i;
  int **j;
  j = (int**)malloc(sizeof(int*)+7);
  *(j + 3) = (int *)malloc(sizeof(int));
  int *k;
  i = *(j + 3);
  *j = k;

  __goblint_check(**j == *k);
  __goblint_check(i == *(j + 3));

  j = &k + 1;

  __goblint_check(j == &k); // FAIL
}
