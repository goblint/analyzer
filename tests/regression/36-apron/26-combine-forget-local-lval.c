// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
#include <assert.h>

int f(int x) {
  return x + 1;
}

int main(void) {
  int y = 42;

  y = f(42);
  // combine should forget caller's y before unifying with y == 43 to avoid bottom

  assert(y);
  return 0;
}
