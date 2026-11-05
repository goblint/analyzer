// PARAM: --enable ana.float.interval --enable ana.int.interval
#include <math.h>
#include <goblint.h>

int main() {
  int i;
  double d;

  // NaN compares unequal to every value
  if ((double)i != NAN) {
    __goblint_check(1);
  } else {
    __goblint_check(0); // NOWARN
  }

  if ((double)i == NAN) {
    __goblint_check(0); // NOWARN
  } else {
    __goblint_check(1);
  }

  if (d != NAN) {
    __goblint_check(1);
  } else {
    __goblint_check(0); // NOWARN
  }

  if (d == NAN) {
    __goblint_check(0); // NOWARN
  } else {
    __goblint_check(1);
  }

  // ... including itself
  if (NAN != NAN) {
    __goblint_check(1);
  } else {
    __goblint_check(0); // NOWARN
  }

  __goblint_check(1); // reachable
  return 0;
}
