// Making sure unknown conditions result in evaluating both branches.
#include<assert.h>

extern int * anIntPlease();
int main() {
  int unknown;
  int x,y;
  int *p,*q;
  p = anIntPlease();
  q = anIntPlease();

  assert(unknown);      // UNKNOWN
  assert(unknown == 0); // UNKNOWN

  assert(x);            // UNKNOWN
  assert(y);            // UNKNOWN
  assert(x == y);       // UNKNOWN

  assert(p);            // UNKNOWN
  assert(q);            // UNKNOWN
  assert(p == q);       // UNKNOWN

  if (unknown)
    assert(1);
  else
    assert(1);

  if (x == y)
    assert(1);
  else
    assert(1);

  if (p == q)
    assert(1);
  else
    assert(1);

  p = &x;
  if (p == q)
    assert(1);
  else
    assert(1);

  assert(p == &x);
  if (q == p)
    assert(1);
  else
    assert(1);
  return 0;
}
