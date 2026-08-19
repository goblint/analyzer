// PARAM: --set ana.activated[+] useAfterFree
#include <stdlib.h>

// lookup has neither a definition nor a library specification, so the
// pointer analysis knows nothing of the address it returns. p points to h's
// object or to that unknown address, and the free may end h's object.
extern int *lookup(void);
extern int choose(void);

int main(void) {
  int *h = malloc(sizeof(int));
  int *p = h;
  if (choose())
    p = lookup();
  *h = 1; // NOWARN
  free(p);
  *h = 2; // WARN
  return 0;
}
