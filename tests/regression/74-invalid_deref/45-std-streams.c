// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --disable warn.info
// Simplified from sv-benchmarks/c/Juliet_Test/CWE121_Stack_Based_Buffer_Overflow---s01---CWE121_Stack_Based_Buffer_Overflow__CWE129_fgets_01_good.
#include <stdio.h>
#include <stdlib.h>

int main() {
  int data = -1;
  char inputBuffer[14] = {0};
  // unmodified standard streams are valid FILE pointers provided by the environment
  if (fgets(inputBuffer, 14, stdin) != NULL) // NOWARN
    data = atoi(inputBuffer);
  fprintf(stderr, "%d\n", data); // NOWARN

  int buffer[10] = {0};
  if (data >= 0 && data <= 9)
    buffer[data] = 1; // NOWARN

  FILE *f = (FILE *)data;
  fflush(f); // WARN

  // a standard stream modified by the program is checked like any other pointer
  char small[2];
  stdout = (FILE *)(small + 5);
  fflush(stdout); // WARN
  return 0;
}
