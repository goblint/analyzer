// PARAM: --set ana.activated[+] useAfterFree
#include <stdlib.h>

// Both calls of resize allocate at the one realloc site. The second realloc
// ends the lifetime of the object q still points to, even if it returns the
// same address, so the read through q reads freed memory. The allocation-order
// facts make the new p younger than q, which must not exempt q's read.

static int *resize(int *p) {
  return realloc(p, 2 * sizeof(int));
}

int main(void) {
  int *p = resize(NULL);
  int *q = p;
  p = resize(p);
  free(p);
  return *q; // WARN
}
