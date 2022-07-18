// PARAM: --enable ana.int.interval --disable ana.context.widen --disable ana.base.context.int
#include <assert.h>

int f(int x) {
  if (x)
    return f(x+1);
  else
    return x;
}

int main () {
  int a = f(1);
  assert(!a);
  return 0;
}
