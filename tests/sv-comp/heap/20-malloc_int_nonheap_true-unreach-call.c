#include <stdlib.h>

extern void abort(void);
void reach_error(){}

void __VERIFIER_assert(int cond) {
  if (!(cond)) {
    ERROR: {reach_error();abort();}
  }
  return;
}

int main() {
  int a, b;
  int *x = &a;
  int *y = &b;
  int *p;

  *x = 0;
  *y = 1;

  __VERIFIER_assert(*x == 0);
  __VERIFIER_assert(*y == 1);

  p = x; x = y; y = p;
  __VERIFIER_assert(*x == 1);
  __VERIFIER_assert(*y == 0);

  return 0;
}
