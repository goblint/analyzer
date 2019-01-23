#include <stdlib.h>

// Exhibts broken behavior in the 'new' solver, causes stack overflow
int main(void) {
  int* ptr;

  int a;
  int b;

  if(rand()) {
    ptr = &a;
  }
  else {
    ptr = &b;
  }
}