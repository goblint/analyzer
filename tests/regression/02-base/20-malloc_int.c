#include <stdlib.h>
#include <assert.h>

int main() {
  int *x = malloc(sizeof(int));
  int *y = malloc(sizeof(int));
  int *p;

  *x = 0;
  *y = 1;

  assert(*x == 0); // UNKNOWN
  assert(*y == 1);  // UNKNOWN

  p = x; x = y; y = p;
  assert(*x == 1);  // UNKNOWN
  assert(*y == 0);  // UNKNOWN

  return 0;
}
