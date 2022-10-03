// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
#include <assert.h>

void main() {
  // requires int64, not int
  signed long long x, y, z;
  if (x < y && y < z) {
    __goblint_check(x < y);
    __goblint_check(y < z);
    __goblint_check(x < z);

    if (9223372036854775805 <= x && z <= 9223372036854775807) {
      __goblint_check(x == 9223372036854775805); // TODO (unknown with D, success with MPQ)
      __goblint_check(y == 9223372036854775806); // TODO (unknown with D, success with MPQ)
      __goblint_check(z == 9223372036854775807); // TODO (unknown with D, success with MPQ)

      __goblint_check(x != -3);
      __goblint_check(y != -2);
      __goblint_check(z != -1);
    }
  }
}
