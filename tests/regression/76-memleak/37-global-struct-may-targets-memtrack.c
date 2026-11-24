// CRAM PARAM: --set ana.malloc.unique_address_count 1 --disable sem.malloc.fail --enable ana.sv-comp.enabled --set ana.specification 'CHECK( init(main()), LTL(G valid-memtrack) )'
#include <stdlib.h>

static struct { void *p; } root;

int main(int argc, char **argv) {
  void *a = malloc(1);
  void *b = malloc(1);
  root.p = argc > 1 ? a : b;
  return 0; // Only one of the two unique allocations remains reachable.
}
