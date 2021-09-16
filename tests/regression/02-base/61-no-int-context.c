// PARAM: --enable ana.int.interval --disable exp.widen-context --disable ana.base.context.int
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
