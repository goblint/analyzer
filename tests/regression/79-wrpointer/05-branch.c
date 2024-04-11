// PARAM: --set ana.activated[+] wrpointer
#include <goblint.h>

void main(void) {
  int *i;
  int **j;
  int *k;
  i = *(j + 3);
  *j = k;
  j = &k + 1;
  int *f;
  if (j != &k) {
    f = i;
    printf("branch1");
    __goblint_check(0); // NOWARN (unreachable)
  } else {
    f = k;
    printf("branch2");
    __goblint_check(1); // reachable
  }

  __goblint_check(f == k);
}
