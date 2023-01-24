//PARAM: --set ana.activated[+] tmpSpecial
#include <float.h>
#include <goblint.h>

void main() {
  float f;
  int c;

  

  if ( __builtin_isfinite(f)  ) {
    __goblint_check(f);
  }
  float x;
  x = __builtin_atan2(f, 0.4);
  if (__builtin_isnan(f) && __builtin_isinf(__builtin_inff()))
  {
    f = 0;
  }
}