// PARAM: --set ana.activated[+] wrpointer

#include <goblint.h>
#include <stdlib.h>

int *i;
int **j;

int *f(int **a, int *b) { return *a; }

int main(void) {
  j = (int **)malloc(sizeof(int *));
  *j = (int *)malloc(sizeof(int));
  int *k = f(j, i);

  __goblint_check(k == *j);

  return 0;
}
