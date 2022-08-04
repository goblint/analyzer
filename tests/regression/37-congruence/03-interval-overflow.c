// PARAM: --enable ana.int.interval --enable ana.int.congruence --disable ana.int.def_exc
// Overflow information should be passed from the interval domain to congruences
#include <assert.h>
#include <stdio.h>

int main(){
  signed char r;

  if (r) {
    r = -68;
  } else {
    r = -63;
  }

  signed char k = r - 80;
  __goblint_check(k == 0); //UNKNOWN!

  signed char non_ov = r - 10;
  __goblint_check(non_ov == -78); //UNKNOWN!

  signed char m = r * 2;

  __goblint_check(m == 0); //UNKNOWN!

  signed char l = r + (-80);
  __goblint_check(l == 0); //UNKNOWN!

  int g;

  if (g) {
    g = -126;
  } else {
    g = -128;
  }

  signed char f = g / (-1);
  __goblint_check(f == 1); //UNKNOWN!

  signed char d = -g;
  __goblint_check(d == 1); //UNKNOWN!

  return 0;

}
