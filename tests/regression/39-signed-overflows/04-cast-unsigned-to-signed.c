// PARAM: --enable ana.int.interval --set sem.int.signed_overflow assume_none
#include <assert.h>

int main(void) {
  unsigned long x;
  long y = x;
  assert(y >= 0); // UNKNOWN!
  return 0;
}
