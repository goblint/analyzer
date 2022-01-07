// Made after 02 22

// PARAM: --set ana.int.interval true --enable ana.base.partition-arrays.enabled

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

  assert(*x == 0);
  assert(*y == 1); //UNKNOWN

  p = x; x = y; y = p;
  assert(*x == 1); //UNKNOWN
  assert(*y == 0);

  return 0;
}
