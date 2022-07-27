// PARAM: --set ana.malloc.unique_address_count 1

// Copied from 11/05. Here, malloc will allocate an unique address for x only.

#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

void* myalloc(size_t s) {
    return malloc(s);
}

int main() {
  int* x = myalloc(sizeof(int));
  int* y = myalloc(sizeof(int));
  int* z = myalloc(sizeof(int));

  *x = 0;
  *y = 1;
  *z = 0;

  assert(*x == 0);
  assert(*y == 1); // UNKNOWN!
}
