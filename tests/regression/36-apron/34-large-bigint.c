// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
#include <assert.h>

void main() {
  // requires bigint, not int64
  unsigned long long x, y, z;
  if (x < y && y < z) {
    assert(x < y);
    assert(y < z);
    assert(x < z);

    if (18446744073709551612ull <= x && z <= 18446744073709551615ull) {
      assert(18446744073709551612ull <= x); // TODO (unknown with D, success with MPQ)
      assert(x <= 18446744073709551613ull); // TODO (unknown with D, success with MPQ)
      assert(18446744073709551613ull <= y); // TODO (unknown with D, success with MPQ)
      assert(y <= 18446744073709551614ull); // TODO (unknown with D, success with MPQ)
      assert(18446744073709551614ull <= z); // TODO (unknown with D, success with MPQ)
      assert(z <= 18446744073709551615ull); // TODO (unknown with D, success with MPQ)

      assert(x >= x - x); // avoid base from answering to check if apron doesn't say x == -3
      assert(y >= y - y); // avoid base from answering to check if apron doesn't say y == -3
      assert(z >= z - z); // avoid base from answering to check if apron doesn't say z == -3
    }
  }
}
