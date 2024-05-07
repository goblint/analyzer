// PARAM: --set ana.activated[+] wrpointer --set ana.activated[+] startState

#include <goblint.h>

void main(void) {
  long *y = (long *)malloc(4 * sizeof(long));
  long a;
  long b;
  long *x = (long *)malloc(4 * sizeof(long));
  int top;

  if (top) {
    *(x + 2) = a + 1;
    *(y + 1) = a + 2;
  } else {
    *(x + 2) = b + 2;
    *(y + 1) = b + 3;
  }

  __goblint_check(*(x + 2) == *(y + 1) - 1);
}
