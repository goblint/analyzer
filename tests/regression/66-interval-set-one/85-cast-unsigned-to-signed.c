// PARAM: --enable ana.int.interval_set --set sem.int.signed_overflow assume_none
#include <goblint.h>

int main(void) {
  unsigned long x;
  long y = x;
  __goblint_check(y >= 0); // UNKNOWN!
  return 0;
}
