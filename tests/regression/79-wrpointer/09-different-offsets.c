// PARAM: --set ana.activated[+] wrpointer --set ana.activated[+] startState
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
