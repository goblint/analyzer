// PARAM: --sets solver td3 --enable ana.int.interval --enable exp.widen-context
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
