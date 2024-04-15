// PARAM: --set ana.activated[+] wrpointer
#include <goblint.h>
#include <stdlib.h>

int *f(int **j) {
  int *i = (int *)malloc(sizeof(int));

  *j = i;

  return i;
}

int main(void) {
  int *i;
  int **j = (int**)malloc(sizeof(int*));
  *j = (int *)malloc(sizeof(int));
  int *k = f(j);

  __goblint_check(k == *j);

  return 0;
}
