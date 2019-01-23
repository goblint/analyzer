#include <stdlib.h>
int main(void) {
  int array1[10000000];
  int array2[10000000];
  int* ptr;

  if(rand()) {
    ptr = &array1;
  }
  else {
    ptr = &array2;
  }

/**
  *ptr = 5;
  ptr++;
  *ptr = 5;


  int val = *ptr+345;
  
  int b = 8;
  array1[7] = 17; **/
}