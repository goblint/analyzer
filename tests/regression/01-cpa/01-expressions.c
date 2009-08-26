#include<stdio.h>
#include<assert.h>

int glob1;
int glob2 = 9;

int main() {
  int i,j,k;

  // simple assignments
  i = 5;
  j = 6;
  k = i + j;
  assert(i+j == 11);

  // global variables
  glob1 = 5;
  assert(glob1+glob2 == 14);

  // simple arithmetic
  i = -j;
  assert(i == -6);

  i = 10 + j;
  assert(i == 16);

  i = 10 - j;
  assert(i == 4);

  i = 3 * j;
  assert(i == 18);

  i = 47 / j;
  assert(i == 7);
  
  i = 8 % j;
  assert(i == 2);


  // comparison operators
  i = 3; j = 7;

  assert(i < j);
  assert(!(i < i));
  assert(!(j < i));

  assert(!(i > j));
  assert(!(i > i));
  assert(j > i);

  assert(i <= j);
  assert(i <= i);
  assert(!(j <= i));

  assert(!(i >= j));
  assert(i >= i);
  assert(j >= i);

  assert(! (i==j));
  assert(i == i);

  assert(i != j);
  assert(!(i != i));

  // boolean expressions
  i = 1; j = 0;

  assert(i);
  assert(!j);

  k =  ! i;
  k += ! j << 1;
  assert(k == 2);

  k =  (i && j);
  k += (j && i) << 1;
  k += (i && i) << 2;
  k += (j && j) << 3;
  assert(k == 4);

  k =  (i || j);
  k += (j || i) << 1;
  k += (i || i) << 2;
  k += (j || j) << 3;
  assert(k == 7);

  return 0;
}
