#include <stdlib.h>
#include <assert.h>

int *x;
int *y;

int main() {
  int *p;
  x = malloc(sizeof(int));
  y = malloc(sizeof(int));

  *x = 0;
  *y = 1;

  assert(*x == 0);
  assert(*y == 1);

  p = x; x = y; y = p;
  assert(*x == 1);
  assert(*y == 0);

  return 0;
}
