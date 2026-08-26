// PARAM: --set ana.activated[+] c2po --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts

#include <goblint.h>

int a, b;
int *p, *q;

void repoint(int c) {
  if (c)
    q = &a;
  else
    q = &b;
}

int main(int argc, char **argv) {
  q = &a;
  p = q;
  repoint(argc);
  __goblint_check(p == q); // UNKNOWN!
  return 0;
}
