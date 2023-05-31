// PARAM: --set ana.malloc.wrappers "['myalloc']"

// Copied & modified from 02/20.
#include <stdlib.h>
#include <goblint.h>

void *myalloc(size_t n) {
  return malloc(n);
}

int main() {
  int *x = myalloc(sizeof(int));
  int *y = myalloc(sizeof(int));
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
