#include <stdint.h>
#include <goblint.h>

// Immediate pointer-integer round trips should retain their target.
// An unknown result used to lose both the alias and the write through it.
int main(void) {
  int value = 1;
  int *p = &value;
  int *q = (int *)(uintptr_t)p;
  __goblint_check(q == p);
  *q = 2;
  __goblint_check(value == 2);

  // Same with signed
  q = (int *)(intptr_t)p;
  __goblint_check(q == p);
  *q = 3;
  __goblint_check(value == 3);

  // With smaller intermediate integer
  q = (int *)(short)p;
  __goblint_check(q == p); // UNKNOWN!
  *q = 4; // WARN (NULL)

  return 0;
}
