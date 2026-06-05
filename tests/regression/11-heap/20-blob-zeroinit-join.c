// PARAM: --set ana.malloc.wrappers[+] myalloc
#include <stdlib.h>
#include <goblint.h>

// By declaring myalloc as wrapper, both calloc and malloc blob have same node (from main) and thus blobs with different zeroinit values get joined
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
  __goblint_check(1); // reachable
  return 0;
}
