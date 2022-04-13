// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
#include <assert.h>

void main() {
  // requires int64, not int
  signed long long x, y, z;
  if (x < y && y < z) {
    assert(x < y);
    assert(y < z);
    assert(x < z);

    if (9223372036854775805 <= x && z <= 9223372036854775807) {
      assert(x == 9223372036854775805); // TODO (unknown with D, success with MPQ)
      assert(y == 9223372036854775806); // TODO (unknown with D, success with MPQ)
      assert(z == 9223372036854775807); // TODO (unknown with D, success with MPQ)

      assert(x != -3);
      assert(y != -2);
      assert(z != -1);
    }
  }
}
