//PARAM: --enable ana.int.wrap_on_signed_overflow
// Enabling ana.int.wrap_on_signed_overflow here, to retain precision for cases when a signed overflow might occcur
#include<stdio.h>
#include<assert.h>

int main() {
  int i,k,j;

  if (k == 5) {
    assert(k == 5);
    return 0;
  }
  assert(k != 5);

  // Signed overflows might occur in some of the following operations (e.g. the first assignment k could be MAX_INT).
  // Signed overflows are undefined behavior, so by default we go to top when they might occur.
  // Here we activated wrap_on_signed_overflow, so we retain precision, assuming that signed overflows are defined (via the usual two's complement representation).

  // simple arithmetic
  i = k + 1;
  assert(i != 6);
  i = k - 1;
  assert(i != 4);
  i = k * 3;       // multiplication with odd numbers is injective
  assert(i != 15);
  i = k * 2;       // multiplication with even numbers is not injective
  assert(i != 10); // UNKNOWN! k could be -2147483643;
  i = k / 2;
  assert(i != 2); // UNKNOWN! k could be 4

  return 0;
}
