// PARAM: --set ana.activated[+] useAfterFree
#include <stdint.h>
#include <stdlib.h>

// The arithmetic on i loses c's address, so the free is given the unknown
// pointer and may free c's object, which the access through c then uses.
int main(void) {
  int *c = malloc(sizeof(int));
  uintptr_t i = (uintptr_t) c;
  i = i ^ 1;
  i = i ^ 1;
  *c = 0; // NOWARN
  free((void *) i);
  *c = 1; // WARN
  return 0;
}
