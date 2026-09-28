// PARAM: --disable warn.imprecise --enable cil.addNestedScopeAttr
#include <stdio.h>
#include <stdlib.h>

// Storage a stream still holds as its buffer ends: a later use of the stream,
// or the flush when the program exits, accesses it after its lifetime.

void helper(void) {
  char local[BUFSIZ];
  setvbuf(stderr, local, _IOFBF, sizeof local); // NOWARN
  fputs("x", stderr);
  return; // WARN
}

void nested(void) {
  {
    char inner[BUFSIZ]; // ends with the block, which no check sees
    setvbuf(stdin, inner, _IOFBF, sizeof inner); // ASSUMED libc.stream-buffer-lifetime
    fclose(stdin);
  }
}

int main(void) {
  helper();
  nested();
  char *heap = malloc(BUFSIZ);
  setvbuf(stdout, heap, _IOFBF, BUFSIZ);
  printf("a");
  free(heap); // WARN
  char buffer[BUFSIZ];
  setvbuf(stdout, buffer, _IOFBF, sizeof buffer); // NOWARN
  printf("b");
  return 0; // WARN (the benchmarks' defect: main's buffer ends before exit flushes stdout)
}
