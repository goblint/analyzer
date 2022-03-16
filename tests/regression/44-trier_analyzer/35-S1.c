#include <assert.h>

extern int printf(char *, ...);

struct bad {int cont; struct bad *next;};

void proc (struct bad *z) {
  assert(z->cont == 1);
  printf ("%d\n",z -> cont);
}

main () {
  struct bad one, two;
  one.cont = 1;
  one.next = &two;
  two.cont = 2;
  two.next = &one;
  proc(&one);
}
