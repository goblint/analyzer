// PARAM: --set ana.activated[+] c2po --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts --set ana.c2po.askbase false
#include <stdlib.h>
#include <goblint.h>

struct Pair {
  int *first;
  int *second;
};

void main(void) {
  int *x;
  struct Pair p;
  p.first = x;

  struct Pair p2;
  p2.first = x;

  __goblint_check(p.first == p2.first);

}
