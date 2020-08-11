// PARAM: --enable ana.int.interval
#include <assert.h>

int main() {
  int x, y, z;
  if (x+1 == 2) {
    assert(x == 1);
  } else {
    assert(x != 1);
  }
  if (5-x == 3)
    assert(x == 2);
  if (5-x == 3 && x+y == x*3)
    assert(x == 2 && y == 4);
  if (x == 3 && y/x == 2)
    assert(y == 6);
  if (y/x == 2 && x == 3)
    assert(x == 3); // TODO y == 6
  if (2+(3-x)*4/5 == 6 && 2*y >= x+4)
    assert(x == -2 && y >= 1);
  if (x > 1 && x < 5 && x % 2 == 1)
    assert(x != 2); // [2,4] -> [3,4] TODO x % 2 == 1


  long xl, yl, zl;
  if (xl+1 == 2) {
    assert(xl == 1);
  } else {
    assert(xl != 1);
  }
  if (5-xl == 3)
    assert(xl == 2);
  if (5-xl == 3 && xl+yl == xl*3)
    assert(xl == 2 && yl == 4);
  if (xl == 3 && yl/xl == 2)
    assert(yl == 6);
  if (yl/xl == 2 && xl == 3)
    assert(xl == 3); // TODO yl == 6
  if (2+(3-xl)*4/5 == 6 && 2*yl >= xl+4)
    assert(xl == -2 && yl >= 1);
  if (xl > 1 && xl < 5 && xl % 2 == 1)
    assert(xl != 2); // [2,4] -> [3,4] TODO x % 2 == 1
}
