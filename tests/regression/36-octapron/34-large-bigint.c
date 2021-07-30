// SKIP PARAM: --sets ana.activated[+] octApron
#include <assert.h>

void main() {
  // requires bigint, not int64
  unsigned long long x, y, z;
  if (x < y && y < z) {
    assert(x < y);
    assert(y < z);
    assert(x < z);

    if (18446744073709551612ull <= x && z <= 18446744073709551615ull) {
      assert(18446744073709551612ull <= x);
      assert(x <= 18446744073709551613ull);
      assert(18446744073709551613ull <= y);
      assert(y <= 18446744073709551614ull);
      assert(18446744073709551614ull <= z);
      assert(z <= 18446744073709551615ull);

      assert(x >= x - x); // avoid base from answering to check if octApron doesn't say x == -3
      assert(y >= y - y); // avoid base from answering to check if octApron doesn't say y == -3
      assert(z >= z - z); // avoid base from answering to check if octApron doesn't say z == -3
    }
  }
}
