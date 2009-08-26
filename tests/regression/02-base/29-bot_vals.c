#include <stdlib.h>
#include <assert.h>

int main(void) {
  int x, y[2];
  int unknown;

  x = 0;
  if (unknown){
    x = atoi("10");
  }
  assert(x); // UNKNOWN

//  x = 8;
//  if (unknown)
//    x = y[0];
//
//  assert(x != 8); 

  return 0;
}