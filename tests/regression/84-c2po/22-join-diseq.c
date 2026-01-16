// PARAM: --set ana.activated[+] c2po --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts --set ana.c2po.askbase false
#include <stdlib.h>
#include <goblint.h>

void main(void) {
  long *a;
  long *b;
  long *c;
  long *d = (long *)malloc(4 * sizeof(long));
  long *e = (long *)malloc(4 * sizeof(long));

  long *unknown;

  int top;

  if (a != b + 4 && e != c && c != d) {
    __goblint_check(a != b + 4);
    __goblint_check(e != c);
    __goblint_check(c != d);
    if (top) {
      d = unknown;
      __goblint_check(a != b + 4);
      __goblint_check(e != c);
      __goblint_check(c != d); // UNKNOWN!

    } else {
      e = unknown;
      __goblint_check(a != b + 4);
      __goblint_check(e != c); // UNKNOWN!
      __goblint_check(c != d);
    }
    // JOIN
    __goblint_check(a != b + 4);
    __goblint_check(e != c); // UNKNOWN!
    __goblint_check(c != d); // UNKNOWN!
  }
}
