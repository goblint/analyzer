// PARAM: --set sem.null-pointer.dereference assume_top
#include <assert.h>
#include <stddef.h>

int main() {
  int r; // rand
  int i = 0;
  int *p;

  if (r)
    p = &i;
  else
    p = NULL;

  if (*p == 2) // WARN
    assert(1); // reachable (via UB)
  return 0;
}
