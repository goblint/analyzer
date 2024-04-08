// PARAM: --set ana.activated[+] wrpointer

#include <goblint.h>

int *f(int **a, int *b) { return *a; }

int main(void) {
  int *i;
  int **j;
  int *k = f(j, i);

  __goblint_check(k == *j);

  return 0;
}
