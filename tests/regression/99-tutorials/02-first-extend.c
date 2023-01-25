// PARAM: --set "ana.activated[+]" signs
#include <goblint.h>

int main() {
  int x;
  int unknown;

  if (unknown) {
    x = -5;
  } else {
    x = 0;
  }

  // The above code branches on an uninitialized variable.
  // The value of x could be either -5 or 0.

  __goblint_check(x < 1); // TODO: Thus, this assertion should hold!

  return 0;
}
