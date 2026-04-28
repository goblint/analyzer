// PARAM: --set ana.activated '["gStoreWidening","assert"]'
#include <goblint.h>

int main() {
  int x;
  int unknown;

  if (unknown) {
    x = -5;
  } else {
    x = -7;
  }

  // The above code branches on an uninitialized variable.
  // The value of x could be either -5 or -7.

  __goblint_check(x < 0);

  if(x > 8) {
    // This is unreachable
    x = 10;
  }

  // This assert should also hold, as the assignment to 10 is unreachable.
  __goblint_check(x < 0);

  return 0;
}
