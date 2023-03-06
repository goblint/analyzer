//PARAM: --set sem.int.signed_overflow assume_wraparound
// Setting sem.int.signed_overflow to assume_wraparound here, to retain precision for cases when a signed overflow might occur
#include<stdio.h>
#include <goblint.h>

int main() {
  int i,k,j;

  if (k == 5) {
    __goblint_check(k == 5);
    return 0;
  }
  __goblint_check(k != 5);

  // Signed overflows might occur in some of the following operations (e.g. the first assignment k could be MAX_INT).
  // Signed overflows are undefined behavior, so by default we go to top when they might occur.
  // Here we activated wrap_on_signed_overflow, so we retain precision, assuming that signed overflows are defined (via the usual two's complement representation).

  // simple arithmetic
  i = k + 1;
  __goblint_check(i != 6);
  i = k - 1;
  __goblint_check(i != 4);
  i = k * 3;       // multiplication with odd numbers is injective
  __goblint_check(i != 15);
  i = k * 2;       // multiplication with even numbers is not injective
  __goblint_check(i != 10); // UNKNOWN! k could be -2147483643;
  i = k / 2;
  __goblint_check(i != 2); // UNKNOWN! k could be 4

  return 0;
}
