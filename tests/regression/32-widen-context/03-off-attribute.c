// PARAM: --enable ana.int.interval --enable ana.context.widen
#include <goblint.h>

int f(int x) __attribute__((goblint_context("no-widen"))); // attributes are not permitted in a function definition
int f(int x) {
  if (x)
    return x * f(x - 1);
  else
    return 1;
}

int main () {
  int a = f(10);
  __goblint_check(a == 3628800);
  return 0;
}
