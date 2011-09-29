// Making sure unknown conditions result in evaluating both branches.
#include<assert.h>

int main() {
  int unknown;
  int x,y;
  int *p,*q;

  if (p == q)
    assert(1);
  else
    assert(1);

  return 0;
}
