// PARAM: --enable ana.int.interval --disable ana.context.widen --enable ana.base.context.int
#include <assert.h>

int f(int x) __attribute__((goblint_context("base.no-int"))); // attributes are not permitted in a function definition
int f(int x) {
  if (x)
    return f(x+1);
  else
    return x;
}

int main () {
  int a = f(1);
  __goblint_check(!a);
  return 0;
}
