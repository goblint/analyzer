// extracted from 01/22
#include <assert.h>

int main() {
  int a = 1;
  int b = 1;
  int *x;
  int rnd;

  if (rnd)
    x = &a;
  else
    x = &b;

  assert(*x == 1);
  b = 2;

  assert(a == 1);
  if (*x != 0) { // invariant shouldn't make less precise!
    assert(a == 1);
  }
  return 0;
}