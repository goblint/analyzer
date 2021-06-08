// PARAM: --disable ana.int.def_exc --enable ana.int.interval
// Ensures that the cast_to function handles casting for congruences correctly.
// TODO: Implement appropriate cast_to function, enable for congruences only and adjust test if necessary.
#include <assert.h>
#include <stdio.h>

int main(){
  int c = 128;
  for (int i = 0; i < 1; i++) {
      c = c - 150;
   }
  char k = (char) c;
  printf ("k: %d", k);
  assert (k == -22); //UNKNOWN

  k = k + 150;
  assert (k == 0); //UNKNOWN!

}
