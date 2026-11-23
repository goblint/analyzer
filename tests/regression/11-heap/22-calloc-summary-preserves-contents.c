// PARAM: --set ana.malloc.unique_address_count 1 --disable sem.malloc.fail
#include <goblint.h>
#include <stdlib.h>

static int *alloc(void) {
  return calloc(2, sizeof(int));
}

int main(int argc, char **argv) {
  int *first = alloc(); // Consume the unique address so p and q share a summary.
  __goblint_check(first[0] == 0);
  free(first);
  int *p = alloc();
  p[0] = 17;
  int *q = alloc(); // Zero-initializes q, but must preserve p[0].
  int *r = argc > 1 ? p : q;
  __goblint_check(r[0] == 0); // UNKNOWN!
  free(p);
  free(q);
  return 0;
}
