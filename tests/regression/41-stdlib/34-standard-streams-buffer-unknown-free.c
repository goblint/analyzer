// CRAM
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// A free through a pointer whose target the analysis does not know may free
// the heap buffer stdout holds.
int main(int argc, char **argv) {
  char *b = malloc(BUFSIZ);
  setvbuf(stdout, b, _IOFBF, BUFSIZ);
  fputs("a\n", stdout);
  uintptr_t i = (uintptr_t) b;
  i ^= (uintptr_t) argc;
  i ^= (uintptr_t) argc;
  free((char *) i);
  return 0;
}
