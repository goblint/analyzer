//PARAM: --enable ana.int.interval
#include <stdio.h>
#include <stdlib.h>
#include <goblint.h>

int main () {
  int r = rand();

  __goblint_check(r >= 0);

  return 0;
}
