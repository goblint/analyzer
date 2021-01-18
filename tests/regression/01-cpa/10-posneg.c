//PARAM: --enable ana.int.wrap_on_signed_overflow
#include<stdio.h>
#include<assert.h>

int main() {
  int i,k,j;

  if (k == 5) {
    assert(k == 5);
    return 0;
  }
  assert(k != 5);

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
