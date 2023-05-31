// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
#include <goblint.h>

void main() {
  // requires bigint, not int64
  unsigned long long x, y, z;
  if (x < y && y < z) {
    __goblint_check(x < y);
    __goblint_check(y < z);
    __goblint_check(x < z);

    if (18446744073709551612ull <= x && z <= 18446744073709551615ull) {
      __goblint_check(18446744073709551612ull <= x); // TODO (unknown with D, success with MPQ)
      __goblint_check(x <= 18446744073709551613ull); // TODO (unknown with D, success with MPQ)
      __goblint_check(18446744073709551613ull <= y); // TODO (unknown with D, success with MPQ)
      __goblint_check(y <= 18446744073709551614ull); // TODO (unknown with D, success with MPQ)
      __goblint_check(18446744073709551614ull <= z); // TODO (unknown with D, success with MPQ)
      __goblint_check(z <= 18446744073709551615ull); // TODO (unknown with D, success with MPQ)

      __goblint_check(x >= x - x); // avoid base from answering to check if apron doesn't say x == -3
      __goblint_check(y >= y - y); // avoid base from answering to check if apron doesn't say y == -3
      __goblint_check(z >= z - z); // avoid base from answering to check if apron doesn't say z == -3
    }
  }
}
