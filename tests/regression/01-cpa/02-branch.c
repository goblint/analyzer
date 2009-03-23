#include<stdio.h>
#include<assert.h>

int main() {
  int i,j,k;
  int x;

  // Testing empty if
  if (x) {
  }

  if (x) {
    assert(x != 0);
    i = 5;
    j = 10;
  } else {
    assert(x == 0);
    i = 7;
    j = 10;
  }

  assert(i==5); // UNKNOWN!
  assert(i==7); // UNKNOWN!
  assert(i != 0);
  assert(j == 10);

  if (j) 
    k = 7;
  else 
    k = 8;
  assert(k == 7);

  switch (x) {
    case 5: k = 3 + x; assert (x == 5); break;
    case 6: k = 2 + x; assert (x == 6); break;
    default: k = 8; assert(x != 5); assert( x!= 6);
  }
  assert(k == 8);

  return 0;
}
