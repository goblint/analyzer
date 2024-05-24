// PARAM: --set ana.activated[+] wrpointer --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts

#include <goblint.h>
#include <math.h>
#include <stdlib.h>

void main(void) {
  long *x;
  long *y;
  long *z = malloc(sizeof(long));
  int top;

  if (top) {
    x = z + 7;
    y = z + 3;
  } else {
    x = z + 1;
    y = z + 1;
  }

  __goblint_check(x == z + 7); // UNKNOWN!
  __goblint_check(x == z + 3); // UNKNOWN!
  __goblint_check(x == z + 1); // UNKNOWN!
  __goblint_check(x == z + 1); // UNKNOWN!

  long *x1;
  long *x2;
  long *y1;
  long *y2;

  if (top) {
    x1 = z + 1;
    y1 = z + 2;
    x2 = z + 1;
    y2 = z + 2;
  } else {
    x1 = z + 2;
    y1 = z + 3;
    x2 = z + 4;
    y2 = z + 5;
  }

  __goblint_check(x1 == y1 - 1);
  __goblint_check(x2 == y2 - 1);
}
