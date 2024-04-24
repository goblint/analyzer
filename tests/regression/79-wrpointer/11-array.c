// PARAM: --set ana.activated[+] wrpointer
#include <goblint.h>
#include <stdlib.h>

void main(void) {
  int m[5];

  int **j;
  int *l;
  j = (int **)malloc(sizeof(int *) + 7);
  j[3] = (int *)malloc(sizeof(int));
  int *k;
  l = j[3];
  j[0] = k;
  j[2] = m;

  __goblint_check(**j == *k);
  __goblint_check(l == *(j + 3));
  __goblint_check(j[2] == m);

  j = &k + 1;

  __goblint_check(j == &k); // FAIL
}
