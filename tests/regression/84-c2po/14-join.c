// PARAM: --set ana.activated[+] c2po --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts --set ana.c2po.askbase false

#include <goblint.h>

void main(void) {
  long y;
  long i;
  long x;
  long *z;
  int top;

  if (top) {
    z = -1 + &x;
    y = x;
  } else {
    z = -1 + &x;
    i = x;
  }

  __goblint_check(z == -1 + &x);
  __goblint_check(x == i); // UNKNOWN!
  __goblint_check(y == x); // UNKNOWN!
}
