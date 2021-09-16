// PARAM: --enable ana.int.interval --disable ana.context.widen --disable ana.base.context.int
#include <assert.h>

int f(int x) __attribute__((goblint_context("base.int"))); // attributes are not permitted in a function definition
int f(int x) {
  if (x)
    return x * f(x - 1);
  else
    return 1;
}

int main () {
  int a = f(10);
  assert(a == 3628800);
  return 0;
}
