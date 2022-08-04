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
  __goblint_check(i+j == 11);

  // global variables
  glob1 = 5;
  __goblint_check(glob1+glob2 == 14);

  // simple arithmetic
  i = -j;
  __goblint_check(i == -6);

  i = 10 + j;
  __goblint_check(i == 16);

  i = 10 - j;
  __goblint_check(i == 4);

  i = 3 * j;
  __goblint_check(i == 18);

  i = 47 / j;
  __goblint_check(i == 7);

  i = 8 % j;
  __goblint_check(i == 2);


  // comparison operators
  i = 3; j = 7;

  __goblint_check(i < j);
  __goblint_check(!(i < i));
  __goblint_check(!(j < i));

  __goblint_check(!(i > j));
  __goblint_check(!(i > i));
  __goblint_check(j > i);

  __goblint_check(i <= j);
  __goblint_check(i <= i);
  __goblint_check(!(j <= i));

  __goblint_check(!(i >= j));
  __goblint_check(i >= i);
  __goblint_check(j >= i);

  __goblint_check(! (i==j));
  __goblint_check(i == i);

  __goblint_check(i != j);
  __goblint_check(!(i != i));

  // boolean expressions
  i = 1; j = 0;

  __goblint_check(i);
  __goblint_check(!j);

  k =  ! i;
  k += ! j << 1;
  __goblint_check(k == 2);

  k =  (i && j);
  k += (j && i) << 1;
  k += (i && i) << 2;
  k += (j && j) << 3;
  __goblint_check(k == 4);

  k =  (i || j);
  k += (j || i) << 1;
  k += (i || i) << 2;
  k += (j || j) << 3;
  __goblint_check(k == 7);

  return 0;
}
