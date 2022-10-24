#include<stdio.h>
#include <goblint.h>

int main() {
  int i,j,k;
  int x;

  // Testing empty if
  if (x) {
  }

  if (x) {
    __goblint_check(x != 0);
    i = 5;
    j = 10;
  } else {
    __goblint_check(x == 0);
    i = 7;
    j = 10;
  }

  __goblint_check(i==5); // UNKNOWN!
  __goblint_check(i==7); // UNKNOWN!
  __goblint_check(i != 0);
  __goblint_check(j == 10);

  if (j)
    k = 7;
  else
    k = 8;
  __goblint_check(k == 7);

  switch (x) {
    case 5: k = 3 + x; __goblint_check(x == 5); break;
    case 6: k = 2 + x; __goblint_check(x == 6); break;
    default: k = 8; __goblint_check(x != 5); __goblint_check( x!= 6);
  }
  __goblint_check(k == 8);

  return 0;
}
