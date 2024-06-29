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

  length = 20;
  unsigned int blub = 5;

  if(top) {
    blub = 10;
  }

  for (int i1 = 0; i1 < length; i1++) {
    // Previously, we would warn as the inverse would make a substraction that becomes negative and is
    // outside the range of unsigned int.
    if (i1 < blub + 3) //NOWARN
    {

    }
  }
  return 0;

}
