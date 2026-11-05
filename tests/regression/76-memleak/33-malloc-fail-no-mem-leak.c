// PARAM: --set ana.malloc.unique_address_count 1 --set ana.activated[+] memLeak --enable sem.malloc.fail

#include <stdlib.h>

int main() {
  int *p = malloc(sizeof(int));
  free(p);
  return 0; // NOWARN
}
