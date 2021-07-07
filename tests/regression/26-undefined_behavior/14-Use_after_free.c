// PARAM:  --sets ana.spec.file file.spec
#include <stdio.h>
#include <stdlib.h>

int main() {
  int *a = malloc(sizeof(int));
  if (a != NULL) {
    *a = 1;
    int *b = a;
    free(b); // detects the free even when the pointer is aliased to b
    return *a; // WARN
  }
  return 0;
}
