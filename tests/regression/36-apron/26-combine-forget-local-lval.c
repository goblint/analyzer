// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
#include <goblint.h>

int f(int x) {
  return x + 1;
}

int main(void) {
  int y = 42;

  y = f(42);
  // combine should forget caller's y before unifying with y == 43 to avoid bottom

  __goblint_check(y);
  return 0;
}
