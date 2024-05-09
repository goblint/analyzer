// PARAM: --set ana.activated[+] wrpointer --set ana.activated[+] startState
#include <goblint.h>
#include <stdlib.h>

void main(void) {
  int *i;
  int **j;
  j = (int **)malloc(sizeof(int *) + 7);
  *(j + 3) = (int *)malloc(sizeof(int));
  int *k;
  *j = k;

  __goblint_check(**j != *k + 1);
  __goblint_check(**j != *k + 2);

  if (*i != **(j + 3)) {
    __goblint_check(i != *(j + 3));
    __goblint_check(&i != j + 3);
    j = NULL;
    __goblint_check(i != *(j + 3)); // UNKNOWN
  }

  int *k2 = (int *)malloc(sizeof(int));
  *j = k2;
  k = k2;

  __goblint_check(*j == k);
  __goblint_check(k2 == k);

  int *f1 = (int *)malloc(sizeof(int));
  int *f2 = f2;

  if (*j != f2) {
    __goblint_check(*j != f2);
    __goblint_check(k != f1);
    j = NULL;
    __goblint_check(*j != f2); // UNKNOWN
    __goblint_check(k != f1);
  }
}
