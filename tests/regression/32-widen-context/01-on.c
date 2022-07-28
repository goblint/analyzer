// PARAM: --set solver td3 --enable ana.int.interval --enable ana.context.widen
#include <assert.h>

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
