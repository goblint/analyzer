#include <goblint.h>
#include <limits.h>

// There are no 128bit integer literals...
__int128 int128max = ((__int128) LLONG_MAX) << 64 | ULLONG_MAX;

int main() {
  __int128 x, y, z;
  z = x + y;
  __goblint_check(z < int128max); // UNKNOWN!
  return 0;
}
