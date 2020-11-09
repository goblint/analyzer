// Copied & modified from 02/20.
#include <stdlib.h>
#include <assert.h>

void *myalloc(size_t n) {
  return malloc(n);
}

int main() {
  int *x = myalloc(sizeof(int));
  int *y = myalloc(sizeof(int));
  int *p;

  *x = 0;
  *y = 1;

  assert(*x == 0);
  assert(*y == 1);

  p = x; x = y; y = p;
  assert(*x == 1);
  assert(*y == 0);

  return 0;
}
