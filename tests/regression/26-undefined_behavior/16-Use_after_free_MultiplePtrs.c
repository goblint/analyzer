// PARAM:  --sets ana.spec.file useafterfree.spec --set ana.activated[+] "'spec'"
#include <stdio.h>
#include <stdlib.h>

int main() {
  int *p1 = malloc(sizeof (int));
  int *p2 = p1;
  int *p3 = p2;
  free(p2);
  *p3 = 7; //WARN
  return 0;
}
