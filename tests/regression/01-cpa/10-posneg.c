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
  i = k * 2;
  assert(i != 10);
  i = k / 2;
  assert(i != 2); // UNKNOWN: k could be 4

  return 0;
}
