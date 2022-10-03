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

  __goblint_check(*x == 1);
  b = 2;

  __goblint_check(a == 1);
  if (*x > 1) { // invariant rules out x == &a
    __goblint_check(x == &b); // TODO
    __goblint_check(*x == 2); // TODO
  }
  return 0;
}