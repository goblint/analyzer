// PARAM: --set ana.activated[+] c2po --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts --set ana.c2po.askbase false

#include <stdlib.h>
#include <goblint.h>

int main(void) {
  long *a;
  long *b;
  long *c;
  long *d = (long *)malloc(4 * sizeof(long));
  long *e = (long *)malloc(4 * sizeof(long));

  long *unknown;

  int top;

  if (a != b && e != c && c != d) {
    __goblint_check(a != b);
    __goblint_check(e != c);
    __goblint_check(c != d);
    if (top) {
      d = unknown;
      d = c + 1;
      __goblint_check(a != b);
      __goblint_check(e != c);
      __goblint_check(c != d); // implicit disequality
    } else {
      e = unknown;
      __goblint_check(a != b);
      __goblint_check(e != c); // UNKNOWN!
      __goblint_check(c != d);
    }
    // JOIN
    __goblint_check(a != b);
    __goblint_check(e != c); // UNKNOWN!
    __goblint_check(c != d);
  }
  return 0;
}
