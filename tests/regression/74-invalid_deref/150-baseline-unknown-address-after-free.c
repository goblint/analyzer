// PARAM: --set ana.activated[+] useAfterFree
#include <stdlib.h>

// lookup has neither a definition nor a library specification, so the
// pointer analysis knows nothing of the address it returns: it may be a's.
extern int *lookup(void);

int main(void) {
  int *a = malloc(sizeof(int));
  int *b = lookup();
  if (b == NULL)
    return 1;
  *b = 1; // NOWARN (nothing has been freed yet)
  free(a);
  *b = 2; // WARN (b may point to a's freed object)
  return 0;
}
