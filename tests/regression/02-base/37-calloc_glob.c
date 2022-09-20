// Made after 02 22

// PARAM: --set ana.int.interval true --set ana.base.arrays.domain partitioned

#include <stdlib.h>
#include <assert.h>

int *x;
int *y;

int main() {
  int *p;
  x = calloc(1, sizeof(int));
  y = calloc(1, sizeof(int));

  *x = 0;
  *y = 1;

  __goblint_check(*x == 0);
  __goblint_check(*y == 1); //UNKNOWN

  p = x; x = y; y = p;
  __goblint_check(*x == 1); //UNKNOWN
  __goblint_check(*y == 0);

  return 0;
}
