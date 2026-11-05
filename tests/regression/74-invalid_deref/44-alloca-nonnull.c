// PARAM: --enable sem.malloc.fail

#include <stdlib.h>
#include <alloca.h>
#include <goblint.h>

int main() {
  int *p = alloca(sizeof(int));
  __goblint_check(p != NULL);
  *p = 0; // NOWARN
  return 0;
}
