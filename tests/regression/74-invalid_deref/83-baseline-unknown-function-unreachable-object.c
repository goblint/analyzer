// PARAM: --set ana.activated[+] useAfterFree
#include <stdlib.h>

int *unused;

// dispose has neither a definition nor a library specification, but no
// argument, global or earlier call gives it the address of p's object, so it
// cannot free it.
extern void dispose(int *p);

int main(void) {
  int *p = malloc(sizeof(int));
  int *q = malloc(sizeof(int));
  *p = 1; // NOWARN
  dispose(q);
  *p = 2; // NOWARN
  *q = 2; // WARN
  // Control: without a call, a use after free is reported as before.
  int *r = malloc(sizeof(int));
  free(r);
  *r = 3; // WARN
  return 0;
}
