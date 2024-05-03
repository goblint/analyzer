// PARAM: --set ana.activated[+] wrpointer --set ana.activated[+] startState

#include <goblint.h>
#include <stdlib.h>

int *i;
int **j;

int *f(int **a, int *b) { return *a; }

int *g(int **a, int *b) {
  a = (int **)malloc(sizeof(int *));
  return *a;
}

int main(void) {

  j = (int **)malloc(sizeof(int *));
  *j = (int *)malloc(sizeof(int));
  int *k = f(j, i);

  __goblint_check(k == *j);

  k = g(j, i);

  __goblint_check(k == *j); // UNKNOWN!

  return 0;
}
