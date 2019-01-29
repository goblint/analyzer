#include <stdlib.h>

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

  *ptr = 5;
  // After this, both a and b might or might not have been updated
}