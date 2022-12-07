#include <goblint.h>

extern int printf(char *, ...);
extern int scanf(char *, ...);

void q (int *);

struct bad {int cont; int *away;};

void p (int *i) {
  if (*i) {
    __goblint_check(i != 0);
    printf ("%d\n",*i);
  }
  else {
    int a;
    q(&a);
  }
}

void q (int *z) {
  struct bad x;
  x.cont = 1;
  x.away = z;
  p(&(x.cont));
}

main () {
  int i;
  scanf("%d",&i);
  p(&i);
}
