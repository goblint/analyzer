//PARAM: --enable ana.float.interval --set ana.activated[+] tmpSpecial
#include <float.h>
#include <math.h>
#include <goblint.h>

void main() {
  float f;
  double d;
  long double ld;

  if(ceilf(f) == 5.f) {
    __goblint_check(f >= 4.f);
    __goblint_check(f > 4.f);
    __goblint_check(f >= 4.5f);  // UNKNOWN!
  }
  if(floorf(f) == 5.f) {
    __goblint_check(f <= 6.f);
    __goblint_check(f < 6.f);
    __goblint_check(f <= 5.5f);  // UNKNOWN!
  }

  if(ceil(d) == 5.) {
    __goblint_check(d >= 4.);
    __goblint_check(d > 4.);
    __goblint_check(d <= 4.5);  // UNKNOWN!
  }
  if(floor(d) == 5.) {
    __goblint_check(d <= 6.);
    __goblint_check(d < 6.);
    __goblint_check(d <= 5.5);  // UNKNOWN!
  }

  if(ceill(ld) == 5.l) {
    __goblint_check(ld >= 4.l);
    __goblint_check(ld > 4.l);    // UNKNOWN
    __goblint_check(ld >= 4.5l);  // UNKNOWN!
  }
  if(floorl(ld) == 5.l) {
    __goblint_check(ld <= 6.l);
    __goblint_check(ld < 6.l);    // UNKNOWN
    __goblint_check(ld <= 5.5l);  // UNKNOWN!
  }

  // Edge cases:
  // 9007199254740992.0 = 2^53; up to here all integer values are representable in double.
  // 2^53+1 is the first that is not representable as double, only as a long double
  long double max_int_l = 9007199254740992.0l;

  if(floorl(ld) == max_int_l) {
    //floorl(ld) == 2^53 => ld in [2^53, 2^53 + 1.0]. This is not representable in double, so Goblint computes with ld in [2^53, 2^53 + 2.0]
    __goblint_check(ld <= (max_int_l + 2.0l));
    // as long as we abstract long doubles with intervals of doubles, the next should be UNKNOWN.
    __goblint_check(ld <= (max_int_l + 1.0l));    // UNKNOWN
  }
  if(ceill(ld) == - max_int_l) {
    // analogous to explanation above but with negative signbit
    __goblint_check(ld >= (- max_int_l - 2.0l));
    // as long as we abstract long doubles with intervals of doubles, the next should be UNKNOWN
    __goblint_check(ld >= (- max_int_l - 1.0l));    // UNKNOWN
  }

  // 4503599627370496.0 = 2^52; from here up to 2^53 double is not able to represent any fractional part, i.e., only integers
  // 2^52 + 0.5 is not representable as double, only as long double
  long double no_fractional_l = 4503599627370496.0l;
  
  if(floorl(ld) == no_fractional_l) {
    // floorl(ld) == 2^52 => ld < 2^52 + 1.0. 
    // If ld were a double, Goblint could compute with ld < pred(2^52 + 1.0), since we know no double can exist between pred(2^52 + 1.0) and 2^52 + 1.0.
    // However for long double this does not hold, ase e.g. (2^52 + 0.5) is representable.
    __goblint_check(ld <= (no_fractional_l + 1.0l));
    // as long as we abstract long doubles with intervals of doubles, the next should be UNKNOWN.
    __goblint_check(ld < (no_fractional_l + 1.0l));    // UNKNOWN
  }
  if(ceill(ld) == - no_fractional_l) {
    // analogous to explanation above but with negative signbit
    __goblint_check(ld >= (- no_fractional_l - 1.0l));
    // as long as we abstract long doubles with intervals of doubles, the next should be UNKNOWN.
    __goblint_check(ld > (- no_fractional_l - 1.0l));    // UNKNOWN
  }

  // same tests, but this time with doubles. Here we can use the knowledge, which values are not representable
  double max_int = (double)max_int_l;
  if(floor(d) == max_int) {
    __goblint_check(d <= (max_int + 2.0));
    __goblint_check(d <= (max_int + 1.0));
  }
  if(ceil(d) == - max_int) {
    __goblint_check(d >= (- max_int - 2.0));
    __goblint_check(d >= (- max_int - 1.0));
  }

  double no_fractional = (double)no_fractional_l;
  if(floor(d) == no_fractional) {
    __goblint_check(d <= (no_fractional + 1.0));
    __goblint_check(d < (no_fractional + 1.0));
  }
  if(ceil(d) == - no_fractional) {
    __goblint_check(d >= (- no_fractional - 1.0));
    __goblint_check(d > (- no_fractional - 1.0));
  }

  // same for float
  float max_int_f = 16777216.0f; // 2^24
  if(floorf(f) == max_int_f) {
    __goblint_check(f <= (max_int_f + 2.0f));
    __goblint_check(f <= (max_int_f + 1.0f));
  }
  if(ceilf(f) == - max_int_f) {
    __goblint_check(f >= (- max_int_f - 2.0f));
    __goblint_check(f >= (- max_int_f - 1.0f));
  }

  float no_fractional_f = 8388608.0f; // 2^23
  if(floorf(f) == no_fractional_f) {
    __goblint_check(f <= (no_fractional_f + 1.0f));
    __goblint_check(f < (no_fractional_f + 1.0f));
  }
  if(ceilf(f) == - no_fractional_f) {
    __goblint_check(f >= (- no_fractional_f - 1.0f));
    __goblint_check(f > (- no_fractional_f - 1.0f));
  }
}
