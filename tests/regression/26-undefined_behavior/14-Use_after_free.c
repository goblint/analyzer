// PARAM:  --sets ana.spec.file useafterfree.spec --set ana.activated[+] "'spec'"
#include <stdio.h>
#include <stdlib.h>

int main() {
  int *a = malloc(sizeof(int));
  if (a != NULL) {
    *a = 1;
    int *b = a;
    free(b); // detects the free even when the pointer is aliased to b
    *b = 7;  // WARN
  }
  return 0;
}
