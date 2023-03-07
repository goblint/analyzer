//PARAM: --enable ana.float.interval --set ana.activated[+] tmpSpecial
#include <float.h>
#include <goblint.h>

void main() {
  double f, g;
  int unk;

  g = __builtin_fabs(f);
  f = 7.;

  if(g == 5.) {
    __goblint_check(f <= 5.); // FAIL
  }
}
