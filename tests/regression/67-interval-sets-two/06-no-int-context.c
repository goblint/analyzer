// PARAM: --enable ana.int.interval_set --disable ana.context.widen --disable ana.base.context.int
#include <goblint.h>

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
