// extracted from 01/22
#include <goblint.h>

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
  if (*x != 0) { // invariant shouldn't make less precise!
    __goblint_check(a == 1);
  }
  return 0;
}