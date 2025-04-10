// PARAM: --set ana.activated[+] c2po --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts --set ana.c2po.askbase false
#include <goblint.h>

int *a, b;
c() { b = 0; }
main() {
  int *d;
  if (a == d)
    ;
  else
    __goblint_check(a != d);
  c();
}
