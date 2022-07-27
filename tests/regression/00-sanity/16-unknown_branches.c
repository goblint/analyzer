// Making sure unknown conditions result in evaluating both branches.
#include<assert.h>

extern int * anIntPlease();
int main() {
  int unknown;
  int x,y;
  int *p,*q;
  p = anIntPlease();
  q = anIntPlease();

  __goblint_check(unknown);      // UNKNOWN
  __goblint_check(unknown == 0); // UNKNOWN

  __goblint_check(x);            // UNKNOWN
  __goblint_check(y);            // UNKNOWN
  __goblint_check(x == y);       // UNKNOWN

  __goblint_check(p);            // UNKNOWN
  __goblint_check(q);            // UNKNOWN
  __goblint_check(p == q);       // UNKNOWN

  if (unknown)
    __goblint_check(1);
  else
    __goblint_check(1);

  if (x == y)
    __goblint_check(1);
  else
    __goblint_check(1);

  if (p == q)
    __goblint_check(1);
  else
    __goblint_check(1);

  p = &x;
  if (p == q)
    __goblint_check(1);
  else
    __goblint_check(1);

  __goblint_check(p == &x);
  if (q == p)
    __goblint_check(1);
  else
    __goblint_check(1);
  return 0;
}
