// PARAM: --set ana.activated[+] wrpointer --set ana.activated[+] startState
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
    f = k;
    printf("branch2");
    __goblint_check(1); // reachable
  } else {
    f = i;
    printf("branch1");
    __goblint_check(0); // NOWARN (unreachable)
  }

  __goblint_check(f == k);

  j = &k;
  if (j != &k) {
    f = k;
    printf("branch1");
    __goblint_check(0); // NOWARN (unreachable)
  } else {
    f = i;
    printf("branch2");
    __goblint_check(1); // reachable
  }

  __goblint_check(f == i);

  if (**j + *k * 23 - 2 * *k == 0 && j != &k) {
    f = k;
    printf("branch1");
    __goblint_check(0); // NOWARN (unreachable)
  } else {
    f = i;
    printf("branch2");
    __goblint_check(1); // reachable
  }
}
