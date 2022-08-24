#include <assert.h>

int main() {
  while (1)
    __goblint_check(0); // FAIL!
  return 0;
}
