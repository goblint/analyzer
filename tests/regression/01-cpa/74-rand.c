//PARAM: --enable ana.int.interval
#include <stdio.h>
#include <stdlib.h>
#include <goblint.h>

int main () {
  int r = rand();

  __goblint_check(r >= 0);
  __goblint_check(r <= RAND_MAX);

  return 0;
}
