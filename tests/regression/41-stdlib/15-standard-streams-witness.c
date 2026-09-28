// CRAM
#include <stdio.h>

// A stream object's value is the buffer the stream uses, not the contents of
// the FILE that f has as its type, so no invariant dereferences f or g.
int main(void) {
  FILE *f = stdout;
  FILE *g = stderr;
  int x = 1;
  fputs("a", f);
  return x;
}
