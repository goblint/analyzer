// PARAM: --enable ana.int.interval --disable ana.context.widen --set ana.context.no_fun[+] f
#include <goblint.h>

int f(int x) {
  if (x)
    return f(x+1);
  else
    return x;
}

int main () {
  int a = f(1);
  __goblint_check(!a); // UNKNOWN (intended: context-insensitive due to ana.context.no_fun option)
  return 0;
}
