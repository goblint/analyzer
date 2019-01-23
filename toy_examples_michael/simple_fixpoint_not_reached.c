#include <stdlib.h>
int main(void) {
  int array1[10000000];
  int array2[10000000];
  int array3[500];
  int* ptr;

  if(rand()) {
    ptr = &array1;
  }
  else {
    ptr = &array2;
  }

  *ptr = 5;
  int v = array3[*ptr];
}