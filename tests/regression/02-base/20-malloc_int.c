#include <stdlib.h>
#include <assert.h>

int main() {
  int *x = malloc(sizeof(int));
  int *y = malloc(sizeof(int));
  int *p;

  *x = 0;
  *y = 1;

  __goblint_check(*x == 0);
  __goblint_check(*y == 1);

  p = x; x = y; y = p;
  __goblint_check(*x == 1);
  __goblint_check(*y == 0);

  return 0;
}
