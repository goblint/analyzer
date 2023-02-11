//PARAM: --set ana.activated[+] tmpSpecial
#include <float.h>
#include <goblint.h>


void main() {
  double f;
  int unk;

  if (__builtin_isnan(f) ) {
    __goblint_check( __builtin_isfinite(f));
  } else {
    __goblint_check( __builtin_isnan(f));
  }
}