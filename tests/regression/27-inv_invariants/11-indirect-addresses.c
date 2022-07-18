// modified from 27/09
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
  if (*x > 1) { // invariant rules out x == &a
    assert(x == &b); // TODO
    assert(*x == 2); // TODO
  }
  return 0;
}