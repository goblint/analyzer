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
    // y could for example also be 7
    assert(y == 6);  // UNKNOWN!
  if (y/x == 2 && x == 3)
    assert(x == 3); // TODO y == 6
  if (2+(3-x)*4/5 == 6 && 2*y >= x+4)
    // x could also be -3
    assert(x == -2 && y >= 1); //UNKNOWN!
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
    // yl could for example also be 7
    assert(yl == 6); // UNKNOWN!
  if (yl/xl == 2 && xl == 3)
    assert(xl == 3); // TODO yl == 6
  if (2+(3-xl)*4/5 == 6 && 2*yl >= xl+4)
    // xl could also be -3
    assert(xl == -2 && yl >= 1); //UNKNOWN!
  if (xl > 1 && xl < 5 && xl % 2 == 1) {
    // UNKNOWN due to Interval32 not being able to represent long
    // assert(xl != 2); // [2,4] -> [3,4] TODO x % 2 == 1
  }


  short xs, ys, zs;
  if (xs+1 == 2) {
    assert(xs == 1);
  } else {
    // Does not survive the casts inserted by CIL
    // assert(xs != 1);
  }
  if (5-xs == 3)
    assert(xs == 2);
  if (5-xs == 3 && xs+ys == xs*3)
    assert(xs == 2 && ys == 4);
  if (xs == 3 && ys/xs == 2)
    // ys could for example also be 7
    assert(ys == 6); // UNKNOWN!
  if (ys/xs == 2 && xs == 3)
    assert(xs == 3); // TODO yl == 6
  if (2+(3-xs)*4/5 == 6 && 2*ys >= xs+4) {
    // xs could also be -3
    assert(xs == -2 && ys >= 1); //UNKNOWN!
  }
  if (xs > 1 && xs < 5 && xs % 2 == 1) {
    assert(xs != 2);
  }
}
