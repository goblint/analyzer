// PARAM: --set ana.activated[+] wrpointer --set ana.activated[+] startState

#include <stdlib.h>
#include <goblint.h>

void main(void) {
  long y;
  long i;
  long x;
  long *z;
  int top;

  y = x;
  z = -1 + &x;

  while (top) {
    int top2;
    z = (long*)malloc(sizeof(long));
    z = -1 + &x;
    y++;
    top = top2;
  }

  __goblint_check(z == -1 + &x);
  __goblint_check(y == x); // UNKNOWN!
}
