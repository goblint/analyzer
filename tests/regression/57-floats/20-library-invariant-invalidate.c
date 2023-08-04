//PARAM: --enable ana.float.interval --set ana.activated[+] tmpSpecial
#include <float.h>
#include <goblint.h>

void main() {
  double f1, g1;
  double f2, g2;
  double unk_double;
  double f3;

  // example 1:
  g1 = __builtin_fabs(f1);
  f1 = 7.;

  if(g1 == 5.) {
    __goblint_check(f1 <= 5.); // FAIL
  }

  // example 2:
  g2 = __builtin_fabs(f2);
  g2 = unk_double;

  if(g2 == 5.) {
    __goblint_check(f2 <= 5.); // UNKNOWN!
  }

  // example 3:
  // the check is not interesting, this only exists to make sure the analyzer can handle this case and terminates
  f3 = __builtin_fabs(f3);

  if(f3 == 0.) {
    __goblint_check(f3 <= 5.);
  }
}
