// PARAM: --set ana.activated[+] c2po --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts --set ana.c2po.askbase false

#include <goblint.h>
#include <stdlib.h>

void *g(int **a, int *b) {
  b = (int *)malloc(sizeof(int *));
  *a = b;
}

int main(void) {
  int *i = (int *)malloc(sizeof(int));
  int **j;
  j = (int **)malloc(sizeof(int *));
  *j = (int *)malloc(sizeof(int));
  int *k = *j;

  __goblint_check(k == *j);

  g(j, i);

  __goblint_check(k == *j); // UNKNOWN!

  return 0;
}
