#include <stdlib.h>

static int *alloc(void) {
  return malloc(sizeof(int));
}

int main(void) {
  int *p = alloc();
  int *q = alloc();
  int *r = alloc();

  return 0; // CRAM: evaluate the disjunction while all three objects are live.
}
