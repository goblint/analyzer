// Made after 02 21
// PARAM: --set ana.int.interval_set true --set ana.base.arrays.domain partitioned

#include <stdlib.h>
#include <goblint.h>

int main() {
  int* x[10];
  int i = 0;

  while (i < 10)
    x[i++] = calloc(1, sizeof(int));

  *x[3] = 50;
  *x[7] = 100;
  __goblint_check(*x[8] == 100); // UNKNOWN

  return 0;
}
