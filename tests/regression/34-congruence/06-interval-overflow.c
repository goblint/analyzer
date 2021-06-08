// PARAM: --enable ana.int.interval --disable ana.int.def_exc
// Overflow + underflow information should be passed from the interval domain to congruences
// TODO Implement underflow + overflow information passing from intervals to congruences and enable congruences + adjust this test if required.
#include <assert.h>
#include <stdio.h>

int main(){
  char r = -128;

  for (int i = 0; i < 5; i++) {
      r = r + 10;
  }

  char k = r - 51;
  assert (k == 0); //UNKNOWN!

  char m = r * 2;
  assert (m == 0); //UNKNOWN!

  char l = r + (-51);
  assert (l == 0); //UNKNOWN!

  int g = -124;

  for (int i = 0; i < 2; i ++) {
      g = g - 2;
  }

  char f = g / (-1);
  assert (f == 1); //UNKNOWN!

  return 0;

}
