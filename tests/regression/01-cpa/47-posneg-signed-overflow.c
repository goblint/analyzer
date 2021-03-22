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

  // simple arithmetic
  i = k + 1;
  assert(i != 6); // UNKNOWN!
  i = k - 1;
  assert(i != 4); // UNKNOWN!
  i = k * 3;
  assert(i != 15); // UNKNOWN!
  i = k * 2;
  assert(i != 10); // UNKNOWN! k could be -2147483643;
  i = k / 2;
  assert(i != 2); // UNKNOWN! k could be 4

  return 0;
}
