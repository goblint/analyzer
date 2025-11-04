// PARAM: --enable ana.int.interval --set sem.int.signed_overflow assume_wraparound
// With sem.int.signed_overflow set to assume_wraparound to assume two's complement representation for signed ints and don't go to top on every signed overflow.
#include <goblint.h>

#define RANGE(x, l, u) x >= l && x <= u

int main() {
  main2();


  int x, y;
  if (x+1 == 2) {
    __goblint_check(x == 1);
  } else {
    __goblint_check(x != 1);
  }
  if (5-x == 3)
    __goblint_check(x == 2);
  else
    __goblint_check(x != 2);
  if (5-x == 3 && x+y == x*3)
    __goblint_check(x == 2 && y == 4);
  if (x == 3 && y/x == 2) {
    __goblint_check(y == 6); // UNKNOWN!
    __goblint_check(RANGE(y, 6, 8));
  }
  if (y/3 == -2)
    __goblint_check(RANGE(y, -8, -6));
  if (y/-3 == -2)
    __goblint_check(RANGE(y, 6, 8));
  if (y/x == 2 && x == 3)
    __goblint_check(x == 3); // TO-DO y == [6,8]; this does not work because CIL transforms this into two if-statements
  if (2+(3-x)*4/5 == 6 && 2*y >= x+5)
    __goblint_check(RANGE(x, -3, -2) && y >= 1); // UNKNOWN
  if (x > 1 && x < 5 && x % 2 == 1) // x = [2,4] && x % 2 = 1 => x = 3
    __goblint_check(x == 3);


  long xl, yl, zl;
  if (xl+1 == 2) {
    __goblint_check(xl == 1);
  } else {
    __goblint_check(xl != 1);
  }
  if (5-xl == 3)
    __goblint_check(xl == 2);
  if (5-xl == 3 && xl+yl == xl*3)
    __goblint_check(xl == 2 && yl == 4);
  if (xl == 3 && yl/xl == 2)
    // yl could for example also be 7
    __goblint_check(yl == 6); // UNKNOWN!
  if (yl/xl == 2 && xl == 3)
    __goblint_check(xl == 3); // TO-DO yl == 6
  if (2+(3-xl)*4/5 == 6 && 2*yl >= xl+4)
    // xl could also be -3
    __goblint_check(xl == -2 && yl >= 1); //UNKNOWN!
  if (xl > 1 && xl < 5 && xl % 2 == 1) {
    __goblint_check(xl != 2); // [2,4] -> [3,4] TO-DO x % 2 == 1
  }


  short xs, ys, zs;
  if (xs+1 == 2) {
    __goblint_check(xs == 1);
  } else {
    // Does not survive the casts inserted by CIL
    // __goblint_check(xs != 1);
  }
  if (5-xs == 3)
    __goblint_check(xs == 2);
  if (5-xs == 3 && xs+ys == xs*3)
    __goblint_check(xs == 2 && ys == 4);
  if (xs == 3 && ys/xs == 2) {
    // ys could for example also be 7
    __goblint_check(ys == 6); // UNKNOWN!
    __goblint_check(RANGE(ys, 6, 8));
  }
  if (ys/3 == -2)
    __goblint_check(RANGE(ys, -8, -6));
  if (ys/-3 == -2)
    __goblint_check(RANGE(ys, 6, 8));
  if (ys/xs == 2 && xs == 3)
    __goblint_check(xs == 3); // TO-DO yl == 6
  if (2+(3-xs)*4/5 == 6 && 2*ys >= xs+5) {
    // xs could also be -3
    __goblint_check(xs == -2 && ys >= 1); //UNKNOWN!
    __goblint_check(RANGE(xs, -3, -2) && ys >= 1); // UNKNOWN
  }
  if (xs > 1 && xs < 5 && xs % 2 == 1) {
    __goblint_check(xs != 2);
  }

}

int main2() {
  int one = 1;
  int two = 2;
  int three = 3;
  int four = 4;
  int five = 5;
  int six = 6;

  int x, y, z;
  if (x+one == two) {
    __goblint_check(x == one);
  } else {
    __goblint_check(x != one);
  }
  if (five-x == three)
    __goblint_check(x == two);
  if (five-x == three && x+y == x*three)
    __goblint_check(x == two && y == four);
  if (x == three && y/x == two) {
    // y could for example also be 7
    __goblint_check(y == six);  // UNKNOWN!
    __goblint_check(RANGE(y, 6, 8));
  }
  if (y/x == two && x == three)
    __goblint_check(x == three); // TO-DO y == six
  if (two+(three-x)*four/five == six && two*y >= x+four)
    // x could also be -three
    __goblint_check(x == -two && y >= one); //UNKNOWN!
  if (x > one && x < five && x % two == one)
    __goblint_check(x != two); // [two,four] -> [three,four] TO-DO x % two == one

  if (y/three == -two)
    __goblint_check(RANGE(y, -8, -6));
  if (y/-three == -two)
    __goblint_check(RANGE(y, 6, 8));
  if (y/x == two && x == three)
    __goblint_check(x == 3); // TO-DO y == [6,8]; this does not work because CIL transforms this into two if-statements
  if (two+(three-x)*four/five == six && two*y >= x+five)
    __goblint_check(RANGE(x, -3, -2) && y >= 1); // UNKNOWN
  if (x > one && x < five && x % two == one) // x = [2,4] && x % 2 = 1 => x = 3
    __goblint_check(x != 2); // [3,4] TO-DO [3,3]



  long xl, yl, zl;
  if (xl+one == two) {
    __goblint_check(xl == one);
  } else {
    __goblint_check(xl != one);
  }
  if (five-xl == three)
    __goblint_check(xl == two);
  if (five-xl == three && xl+yl == xl*three)
    __goblint_check(xl == two && yl == four);
  if (xl == three && yl/xl == two)
    // yl could for example also be 7
    __goblint_check(yl == six); // UNKNOWN!
  if (yl/xl == two && xl == three)
    __goblint_check(xl == three); // TO-DO yl == six
  if (two+(three-xl)*four/five == six && two*yl >= xl+four)
    // xl could also be -three
    __goblint_check(xl == -two && yl >= one); //UNKNOWN!
  if (xl > one && xl < five && xl % two == one) {
    __goblint_check(xl != two); // [two,four] -> [three,four] TO-DO x % two == one
  }


  short xs, ys, zs;
  if (xs+one == two) {
    __goblint_check(xs == one);
  } else {
    // Does not survive the casts inserted by CIL
    // __goblint_check(xs != one);
  }
  if (five-xs == three)
    __goblint_check(xs == two);
  if (five-xs == three && xs+ys == xs*three)
    __goblint_check(xs == two && ys == four);
  if (xs == three && ys/xs == two) {
    // ys could for example also be 7
    __goblint_check(ys == six); // UNKNOWN!
    __goblint_check(RANGE(ys, six, 8));
  }
  if (ys/xs == two && xs == three)
    __goblint_check(xs == three); // TO-DO yl == six
  if (two+(three-xs)*four/five == six && two*ys >= xs+five) {
    // xs could also be -three
    __goblint_check(xs == -two && ys >= one); //UNKNOWN!
    __goblint_check(RANGE(xs, -three, -two) && ys >= one); // UNKNOWN
  }
  if (xs > one && xs < five && xs % two == one) {
    __goblint_check(xs != two);
  }
  if (ys/three == -two)
    __goblint_check(RANGE(ys, -8, -6));
  if (ys/-three == -two)
    __goblint_check(RANGE(ys, 6, 8));
  if (ys/xs == two && xs == three)
    __goblint_check(xs == 3); // TO-DO y == [6,8]; this does not work because CIL transforms this into two if-statements
}
