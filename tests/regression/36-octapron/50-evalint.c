// SKIP PARAM: --sets ana.activated[+] octApron --enable ana.int.interval
#include <assert.h>

void foo(int a, int *pb) {
  int b = *pb;
   // base knows a == 4 and b == 4, octApron only knows a == 4
  assert(a == b);
}

void main() {
  int x, y, z, a, b;
  if (x < y && y < z) {
    // base doesn't know anything, octApron knows x < y < z
    assert(x < y);
    assert(y < z);
    assert(x < z);

    if (3 <= x && z <= 5) {
      // base knows 3 <= x and z <= 5, octApron knows x == 3 and y == 4 and z == 5
      a = y; // base should now know a == 4 via EvalInt query

      assert(x == 3);
      assert(y == 4);
      assert(z == 5);
      assert(a == 4);

      b = 4;
      foo(a, &b); // base should add a == 4 and b == 4 to context, octApron only adds a == 4
    }
  }
}
