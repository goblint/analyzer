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

  *ptr = 5;
  // after this, it is incorrect to have
  //    array1: part by ptr-array1 (\bot, 5, \top)
  //    array2: part by ptr-array2 (\bot, 5, \top)
  // sth that would be correct is e.g.
  //    array1: part by ptr-array1 (\bot, \top, \top)
  //    array2: part by ptr-array2 (\bot, \top, \top)  
}