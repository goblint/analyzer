// PARAM: --set ana.malloc.unique_address_count 1

// Copied from 11/05. Here, variable y is not unique and cannot be strongly updated.

#include <stdlib.h>
#include <stdint.h>

void* myalloc(size_t s) {
    return malloc(s);
}

int main() {
  int* x = myalloc(sizeof(int));
  int* y = myalloc(sizeof(int));
  int *p;

  *x = 0;
  *y = 1;

  *x = 2;
  *y = 3;

  assert (*x == 2);
  assert (*y == 3); // UNKNOWN!
}
