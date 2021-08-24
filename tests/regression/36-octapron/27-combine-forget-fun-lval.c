// SKIP PARAM: --sets ana.activated[+] octApron
#include <assert.h>

int f(int x) {
  return x + 1;
}

int main(void) {
  int y = 42;

  y = f(y);
  // combine should forget callee's y after substituting arg vars with args to avoid bottom in #ret substitute

  assert(y);
  return 0;
}
