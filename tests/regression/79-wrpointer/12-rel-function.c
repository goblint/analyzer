// PARAM: --set ana.activated[+] wrpointer --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts

#include <goblint.h>
#include <stdlib.h>

void *f(int **a, int **b) {
  int *j;
  int **i = &j;
  j = (int *)malloc(sizeof(int) * 2);
  *a = j;
  *b = *i + 1;
}

int main(void) {
  int **c = (int**)malloc(sizeof(int*));
  int **d = (int**)malloc(sizeof(int*));;
  f(c, d);

  __goblint_check(*d == *c + 1);

  return 0;
}
