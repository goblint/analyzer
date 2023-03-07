//PARAM: --enable ana.float.interval --set ana.activated[+] tmpSpecial
#include <float.h>
#include <math.h>
#include <goblint.h>

void main() {
  double f, g;
  double x;
  int unk;

  // isnan, isfinite
  if(__builtin_isfinite(f)) {
    __goblint_check(__builtin_isfinite(f));
    __goblint_check(! __builtin_isnan(f));
  }
  if(__builtin_isnan(f)) {
    __goblint_check(__builtin_isnan(f));
    __goblint_check(! __builtin_isfinite(f));
  }

  // Comparison
  x = (unk) ? -100. : 100.;
  if(__builtin_isgreater(x, 0.)) {
    __goblint_check(x > 0.);
  }
  if(__builtin_isgreaterequal(x, 0.)) {
    __goblint_check(x >= 0.);
  }
  if(__builtin_isless(x, 0.)) {
    __goblint_check(x < 0.);
  }
  if(__builtin_islessequal(x, 0.)) {
    __goblint_check(x <= 0.);
  }
  if(__builtin_islessgreater(x, 0.)) {
    __goblint_check(x < 0. || x > 0.); // UNKNOWN
  }

  // fabs
  if(__builtin_fabs(f) == 4.) {
    __goblint_check(f >= -4.);
    __goblint_check(f <= 4.);
  }
  g = (unk) ? (3.) : (5.);
  if(__builtin_fabs(f) == g) {
    __goblint_check(f >= -5.);
    __goblint_check(f <= 5.);
  }
  if(__builtin_fabs(f) == -6.) {
    // DEAD
    g = 0.;
  }

  // ceil, floor
  if(ceil(f) == 5.) {
    __goblint_check(f <= 5.);
    __goblint_check(f > 4.);
    __goblint_check(f >= 4.5);  // UNKNOWN!
  }
  if(floor(f) == 5.) {
    __goblint_check(f >= 5.);
    __goblint_check(f < 6.);
    __goblint_check(f >= 5.5);  // UNKNOWN!
  }
}
