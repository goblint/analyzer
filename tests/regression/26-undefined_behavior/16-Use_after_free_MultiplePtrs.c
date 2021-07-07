// PARAM:  --sets ana.spec.file useafterfree.spec --set ana.activated[+] "'spec'"
#include <stdio.h>
#include <stdlib.h>

int main() {
  int *a = malloc(sizeof(int));
  if (a != NULL) {
    *a = 1;
    int *p1 = a;
    int *p2 = p1;
    int *p3 = p2;
    free(p2); // detects the free even when the pointer is aliased to b
    *p3 = 7;  // WARN
  }
  return 0;
}
