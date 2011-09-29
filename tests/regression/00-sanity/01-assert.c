// just a few sanity checks on the asserts
#include<assert.h>

int main() {
  int success = 1;
  int silence = 1;
  int fail = 0;
  int unknown;
  assert(success);
  assert(fail); // FAIL!
  assert(unknown == 4); // UNKNOWN!
  return 0;
  assert(silence); // NOWARN!
}
