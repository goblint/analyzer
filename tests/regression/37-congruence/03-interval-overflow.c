// PARAM: --enable ana.int.interval --enable ana.int.congruence --disable ana.int.def_exc
// Overflow + underflow information should be passed from the interval domain to congruences
#include <assert.h>
#include <stdio.h>

int main(){
  char r;

  if (r) {
    r = -68;
  } else {
    r = -63;
  }

  char k = r - 80;
  assert (k == 0); //UNKNOWN!

  char non_ov = r - 10;
  assert (non_ov == -78); //UNKNOWN

  char m = r * 2;

  assert (m == 0); //UNKNOWN!

  char l = r + (-80);
  assert (l == 0); //UNKNOWN!

  int g;

  if (g) {
    g = -126;
  } else {
    g = -128;
  }

  char f = g / (-1);
  assert (f == 1); //UNKNOWN!

  char d = -g;
  assert (d == 1); //UNKNOWN!

  return 0;

}
