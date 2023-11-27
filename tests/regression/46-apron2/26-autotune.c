//SKIP PARAM: --enable ana.int.interval --sets sem.int.signed_overflow assume_none --set ana.activated[+] apron --enable ana.autotune.enabled
// Check that autotuner disables context for apron as well for recursive calls
#include <goblint.h>

int f(int x, int y) {
  __goblint_check(x == y);

  int top;
  if(top) {
    x = x+1;
    y = y+1;
  }

  __goblint_check(x == y);

  if (x)
    return f(x,y);
  else
    return x;
}

int main () {
  int a = f(1,1);
  return 0;
}
