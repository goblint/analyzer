#include <stdlib.h>
#include <string.h>
#include <goblint.h>

void *myalloc(size_t s, _Bool zero) {
  if (zero)
    return calloc(1, s);
  else
    return malloc(s);
}

int main() {
  int r;
  int *p;
  p = myalloc(sizeof(int), r);
  memcpy(p, &r, sizeof(int)); // NOCRASH
  return 0;
}
