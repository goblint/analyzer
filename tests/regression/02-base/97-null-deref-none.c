// PARAM: --set sem.null-pointer.dereference assume_none
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
    assert(0); // NOWARN (unreachable)
  return 0;
}
