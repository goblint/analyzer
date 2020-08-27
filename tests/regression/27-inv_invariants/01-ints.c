// PARAM: --enable ana.int.interval
#include <assert.h>

#define RANGE(x, l, u) x >= l && x <= u

int main() {
  int x, y;
  if (x+1 == 2) {
    assert(x == 1);
  } else {
    assert(x != 1);
  }
  if (5-x == 3)
    assert(x == 2);
  else
    assert(x != 2);
  if (5-x == 3 && x+y == x*3)
    assert(x == 2 && y == 4);
  if (x == 3 && y/x == 2) {
    assert(y == 6); // UNKNOWN!
    assert(RANGE(y, 6, 8));
  }
  if (y/3 == -2)
    assert(RANGE(y, -8, -6));
  if (y/-3 == -2)
    assert(RANGE(y, 6, 8));
  if (y/x == 2 && x == 3)
    assert(x == 3); // TODO y == [6,8]; this does not work because CIL transforms this into two if-statements
  if (2+(3-x)*4/5 == 6 && 2*y >= x+5)
    assert(RANGE(x, -3, -2) && y >= 1);
  if (x > 1 && x < 5 && x % 2 == 1) // x = [2,4] && x % 2 = 1 => x = 3
    assert(x != 2); // [3,4] TODO [3,3]
}
