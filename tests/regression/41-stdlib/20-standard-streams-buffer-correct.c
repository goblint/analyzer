// PARAM: --disable warn.imprecise --enable cil.addNestedScopeAttr
#include <stdio.h>
#include <stdlib.h>

// The correct patterns: a static buffer, which outlives the stream, and a
// stream closed before its buffer ends. (setvbuf(stream, NULL, ...) before
// the end would detach the buffer too, but setvbuf may be called only before
// any other operation on the stream, C11 7.21.5.6.)
static char static_buffer[BUFSIZ];

void closes(void) {
  char local[BUFSIZ];
  setvbuf(stderr, local, _IOFBF, sizeof local);
  fputs("x", stderr);
  fclose(stderr);
  return; // NOWARN
}

int main(void) {
  closes();
  char *heap = malloc(BUFSIZ);
  setvbuf(stdin, heap, _IOFBF, BUFSIZ);
  getchar();
  fclose(stdin);
  free(heap); // NOWARN
  setvbuf(stdout, static_buffer, _IOFBF, sizeof static_buffer);
  printf("a");
  return 0; // NOWARN
}
