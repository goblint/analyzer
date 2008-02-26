// PARAM: --debug
// just a few sanity checks on the asserts
#include<assert.h>

int main() {
  int success = 1;
  int fail = 0;
  int unknown;
  assert(success);
  assert(fail); // FAIL!
  assert_unknown(unknown);
  assert_unknown(fail);  // FAIL!
  return 0;
}
