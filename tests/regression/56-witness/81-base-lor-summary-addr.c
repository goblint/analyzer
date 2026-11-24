// PARAM: --disable sem.malloc.fail --set ana.malloc.unique_address_count 0 --set witness.yaml.validate 81-base-lor-summary-addr.yml
#include <stdlib.h>

static int *alloc(void) {
  return malloc(sizeof(int));
}

int main(void) {
  int *p = alloc();
  int *q = alloc();
  int *r = alloc();

  // All three objects are distinct, even though they share a summary address.
  return 0; // UNKNOWN (witness)
}
