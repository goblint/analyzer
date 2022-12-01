// PARAM: --set "ana.activated[+]" signs
#include <goblint.h>

int main() {
  int x;
  int unknown;

  if (unknown) {
    x = 5;
  } else {
    x = 7;
  }

  // The above code branches on an uninitialized variable.
  // The value of x could be either 5 or 7.

  __goblint_check(x > 0); // TODO: Thus, this assertion should hold!

  return 0;
}
