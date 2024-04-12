//PARAM:
#include <stdio.h>
#include <stdlib.h>
#include <goblint.h>

int main () {
  // Needs to be unsigned as we create ranges corresponding to types and ending for signed
  // means we have int as there are no types containing only negative numbers
  unsigned int r;

  if(r < 1024) {
    __goblint_check(r <= 32768);
  }

  return 0;
}
