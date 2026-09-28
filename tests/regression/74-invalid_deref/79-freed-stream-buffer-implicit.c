// PARAM: --set ana.activated[+] useAfterFree
#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>

// 78-freed-stream-buffer.c with the calls that use stdout without taking it as
// an argument, and one whose specification reaches it only one level.
int main(void) {
  char *b = malloc(BUFSIZ);
  setvbuf(stdout, b, _IOFBF, BUFSIZ);
  printf("a\n"); // NOWARN
  free(b);
  printf("b\n"); // WARN
  puts("c"); // WARN
  fputs_unlocked("d", stdout); // WARN
  putwchar(L'e'); // WARN
  return 0;
}
