// PARAM: --enable ana.int.interval --disable ana.context.widen --set annotation.goblint_context.f[+] no-context
#include <goblint.h>

int f(int x) {
  if (x)
    return f(x+1);
  else
    return x;
}

int main () {
  int a = f(1);
  __goblint_check(!a); // UNKNOWN (intended: context-insensitive due to annotation option)
  return 0;
}
