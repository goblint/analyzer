#include <stdlib.h>
#include <goblint.h>

int main(void) {
  int x, y[2];
  int unknown;

  x = 0;
  if (unknown){
    x = atoi("10");
  }
  __goblint_check(x); // UNKNOWN

//  x = 8;
//  if (unknown)
//    x = y[0];
//
//  __goblint_check(x != 8);

  return 0;
}