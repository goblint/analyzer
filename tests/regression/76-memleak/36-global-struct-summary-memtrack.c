// CRAM PARAM: --set ana.malloc.unique_address_count 1 --disable sem.malloc.fail --enable ana.sv-comp.enabled --set ana.specification 'CHECK( init(main()), LTL(G valid-memtrack) )'
#include <stdlib.h>

static struct { void *p; } root;

static void *alloc(void) {
  return malloc(1);
}

int main(void) {
  free(alloc());     // Consume the unique address at this allocation site.
  root.p = alloc();
  root.p = alloc();  // Only the second of the two summarized objects is reachable.
  return 0;
}
