// PARAM: --set ana.malloc.unique_address_count 2

// Copied from 29/07. Here, unique addresses are allocated for both x and y.
// Therefore, it is not necessary to specify wrapper function.

#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

void* myalloc(size_t s) {
    return malloc(s);
}

int main() {
  int* x = myalloc(sizeof(int));
  int* y = myalloc(sizeof(int));
  int *p;

  *x = 0;
  *y = 1;

  __goblint_check(*x == 0);
  __goblint_check(*y == 1);

  p = x; x = y; y = p;
  __goblint_check(*x == 1);
  __goblint_check(*y == 0);
}
