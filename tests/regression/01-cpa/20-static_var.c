#include<stdio.h>
#include <goblint.h>

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
  __goblint_check(i == 1);
  __goblint_check(x == 0);
  i = statinc();
  __goblint_check(i == 2);
  __goblint_check(x == 0);

  return 0;
}
