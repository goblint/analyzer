// SKIP (just for manually testing that update_suite works)
#include <assert.h>

int main() {
  int x = 42;
  // Should fail with: Expected unknown, but registered success
  __goblint_check(x == 42); // UNKNOWN

  return 0;
}
