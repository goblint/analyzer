// CRAM
#include <stdio.h>
#include <stdlib.h>

// setvbuf hands stdout a heap buffer, so fputs writes it after it is freed.
int main(void) {
  char *b = malloc(BUFSIZ);
  setvbuf(stdout, b, _IOFBF, BUFSIZ);
  fputs("a\n", stdout);
  free(b);
  fputs("b\n", stdout);
  return 0;
}
