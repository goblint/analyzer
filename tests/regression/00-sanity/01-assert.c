// just a few sanity checks on the asserts
#include<assert.h>

int main() {
  int success = 1;
  int fail = 0;
  int unknown;
  assert(success);
  assert(fail); // this should fail
  assert_unknown(unknown);
  assert_unknown(fail);  // this should fail
  return 0;
}
