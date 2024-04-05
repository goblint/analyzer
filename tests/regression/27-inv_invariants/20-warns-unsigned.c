// PARAM: --enable ana.int.interval
#include <stdio.h>
#include <stdlib.h>
#include <time.h>


int main() {
  unsigned int length = 5;
  int i = 0;
  int top;

  if(top) {
    i = 10;
  }

  // Previously, we would warn as invariant internally casts an artificially created top between types.
  if(i < length + 3) //NOWARN
  {
    __goblint_check(i <= 8);
    i = 8;
  }
}
