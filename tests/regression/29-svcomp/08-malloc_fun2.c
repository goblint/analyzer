// PARAM: --set ana.malloc.wrappers "['myalloc','myalloc2']"

// Copied & modified from 02/20.
#include <stdlib.h>
#include <goblint.h>

void *myalloc(size_t n) {
  return malloc(n);
}

void *myalloc2(size_t n) {
  return myalloc(n);
}

int main() {
  int *x = myalloc2(sizeof(int));
  int *y = myalloc2(sizeof(int));
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
