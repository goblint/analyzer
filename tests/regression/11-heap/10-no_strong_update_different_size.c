// PARAM: --set ana.malloc.unique_address_count 2

#include <assert.h>
#include <stdlib.h>
#include <stdint.h>

void* myalloc(size_t s) {
    return malloc(s);
}

int main() {
  int *x = myalloc(2 * sizeof(int));
  int *y = myalloc(2 * sizeof(int));

  *x = 0;
  *y = 1;
  *x = 2; // Size of written value is shorter than blob

  __goblint_check(*x == 2); // UNKNOWN!
}
