// PARAM: --enable ana.int.interval --disable ana.context.widen --disable ana.base.context.int --set annotation.goblint_context.f[+] base.int
#include <assert.h>

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
