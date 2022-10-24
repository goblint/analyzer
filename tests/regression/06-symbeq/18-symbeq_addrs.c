// PARAM: --disable ana.mutex.disjoint_types --set ana.activated[+] "'var_eq'"
#include<stdlib.h>
#include <goblint.h>

typedef struct {
  int x,y;
} S;

int main () {
  int x, y, z, uk;
  int *p, *q, *r;
  S a, b, *ps;

  x = y = z;

  __goblint_check(x == y);
  __goblint_check(x == z);
  __goblint_check(z == y);

  x = uk+10; y = uk+20; z = uk+30;

  x = y;
  x = z;

  __goblint_check(x == z);
  __goblint_check(x == y); // UNKNOWN
  x = 40+uk;

  if (uk) {
    p = &x;
    ps = &a;
  } else {
    p = &y;
    ps = &b;
  }

  y = *p;
  __goblint_check(y == *p);
  p = &z;
  __goblint_check(y == *p); // UNKNOWN
  p = NULL+10;

  r = &ps->x;
  __goblint_check(r == &ps->x);
  ps = &a;
  __goblint_check(r == &ps->x);//UNKNOWN

  return 0;
}
