// PARAM: --set ana.activated[+] wrpointer --set ana.activated[+] startState

#include <goblint.h>
#include <stdlib.h>

int *i;
int **j;

int *f(int **a, int *b) {
  //a=...;//find tainted vars
  return *a; }

int main(void) {

  j = (int **)malloc(sizeof(int *));
  *j = (int *)malloc(sizeof(int));
  int *k = f(j, i);

  __goblint_check(k == *j);

  return 0;
}
