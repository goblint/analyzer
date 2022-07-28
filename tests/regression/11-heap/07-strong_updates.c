// PARAM: --set ana.malloc.unique_address_count 2

// Both variables x and y are unique and can be strongly (destructively) updated.

#include <assert.h>
#include <stdlib.h>
#include <stdint.h>

void* myalloc(size_t s) {
    return malloc(s);
}

int main() {
  int* x = myalloc(sizeof(int));
  int* y = myalloc(sizeof(int));

  *x = 0;
  *y = 1;

  *x = 2;
  *y = 3;

  assert (*x == 2);
  assert (*y == 3);
}
