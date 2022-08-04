// just a few sanity checks on asserts
#include <assert.h>

int main() {
  int success = 1;
  int silence = 1;
  int fail = 0;
  int unknown;
  // intentionally using assert, specific order to work with assert refine
  assert(success);
  assert(unknown == 4); // UNKNOWN!
  assert(fail); // FAIL!
  return 0;
  assert(silence); // NOWARN!
}
