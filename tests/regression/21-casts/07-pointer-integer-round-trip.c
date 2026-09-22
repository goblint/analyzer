// PARAM:
#include <stdint.h>
#include <goblint.h>

// Immediate pointer/integer round trips must retain their dereference target.
// An unknown result used to lose both the alias and the write through it.
int main(void) {
  int value = 1;
  int *p = &value;
  int *q = (int *)(uintptr_t)p;
  __goblint_check(q == p);
  *q = 2;
  __goblint_check(value == 2);

  return 0;
}
