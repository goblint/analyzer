// PARAM: --disable warn.imprecise --disable warn.unsound --enable cil.addNestedScopeAttr
#include <stdio.h>
#include <stdlib.h>

// The analysis tracks the buffer of stdin, stdout and stderr only. A buffer
// handed to any other stream, such as one fopen returned, or to stdout after
// the program assigned it, is not checked at a return or a free, so every
// buffer but static storage is assumed to stay live.
static char static_buffer[BUFSIZ];

int main(void) {
  char local[BUFSIZ];
  FILE *f = fopen("/dev/null", "w");
  if (!f)
    return 1;
  setvbuf(f, local, _IOFBF, sizeof local); // ASSUMED libc.stream-buffer-lifetime
  fclose(f); // NOWARN

  FILE *g = fopen("/dev/null", "w");
  if (!g)
    return 1;
  char *heap = malloc(BUFSIZ);
  setvbuf(g, heap, _IOFBF, BUFSIZ); // ASSUMED libc.stream-buffer-lifetime
  fclose(g);
  free(heap);

  FILE *h = fopen("/dev/null", "w");
  if (!h)
    return 1;
  setvbuf(h, static_buffer, _IOFBF, sizeof static_buffer); // NOWARN
  fclose(h);

  // A standard stream: the return of main is checked, and a heap buffer's free.
  setvbuf(stderr, local, _IOFBF, sizeof local); // NOWARN
  setvbuf(stderr, NULL, _IONBF, 0);
  char *heap2 = malloc(BUFSIZ);
  setvbuf(stderr, heap2, _IOFBF, BUFSIZ); // NOWARN
  return 0;
}
