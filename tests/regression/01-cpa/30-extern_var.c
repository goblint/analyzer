#include <goblint.h>

int p; // assumed to be 0-initialized
extern int q; // not assumed to be 0-initialized

int main() {
  __goblint_check(p == 0);
  __goblint_check(q == 0); // UNKNOWN!

  p = 42;
  q = 42; // does not stick because extern may be modified externally
  __goblint_check(p == 42);
  __goblint_check(q == 42); // UNKNOWN!
  return 0;
}
