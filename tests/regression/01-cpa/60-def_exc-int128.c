// PARAM: --set sem.int.signed_overflow assume_top
#include <goblint.h>
#include <limits.h>

// There are no 128bit integer literals...
__int128 int128max = ((__int128) LLONG_MAX) << 64 | ULLONG_MAX;

int main() {
  __int128 x, y, z;
  z = x + y;
  __goblint_check(z < int128max); // UNKNOWN!

  __goblint_assume(x != 0); // x -> Not{0}[-127,127]
  z = x - int128max; // Underflow, so due to assume_top x -> Unknown[-127,127], not x -> Not{-int128max}[-127,127]
  __goblint_check(z != -int128max); // UNKNOWN!
  return 0;
}
