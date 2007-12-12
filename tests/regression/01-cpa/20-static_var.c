#include<stdio.h>
#include<assert.h>

int statinc() {
  int static x;
  x++;
  return x;
}

int main () {
  int i,j;
  int x = 0;

  i = 0;
  // static variable (thanks to CIL)
  i = statinc();
  assert(i == 1);
  assert(x == 0);
  i = statinc();
  assert(i == 2);
  assert(x == 0);

  return 0;
}
