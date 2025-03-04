// PARAM: --set ana.activated[+] c2po --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts --set ana.c2po.askbase false

#include <goblint.h>
#include <stdlib.h>

int main(void) {
  long y;
  long i;
  long *x = malloc(sizeof(long) * 300);
  long *x2 = x;
  long *z;
  int top;
  top = top % 300; // top is some number that is < 300

  y = *x;
  z = -1 + x;

  while (top > 0) {
    z = (long *)malloc(sizeof(long));
    x++;
    z = -1 + x;
    y++;
    top--;
  }

  __goblint_check(z == -1 + x);
  __goblint_check(y == *x2); // UNKNOWN!

  return 0;
}
