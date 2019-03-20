#include <stdlib.h>
int global_array[50];

int main(void) {
  some_func();
  pretty_awesome();

  int x = global_array[5];
}

void pretty_awesome() {
  int top;

  int arr1[42];
  int arr2[42];
  int *ptr;

  for(int i = 0; i < 42; i++) {
    arr1[i] = 4;
    arr2[i] = 4;
  }

  ptr = &arr1[7];

  if(top) {
    ptr = &arr2[7];
  }

  *ptr = 9;
  
  // Case ptr = &arr1[7]
  //    arr1 -> (ptr-arr1, ([4,4], [9,9],[4,4]))
  //    arr2 -> (-,[4,4])

  // Case ptr = &arr2[7]
  //    arr1 -> (-, [4,4])
  //    arr2 -> (ptr-arr2, ([4,4], [9,9],[4,4]))

  // LUB:
  //    arr1 -> (-, [4,9])
  //    arr2 -> (-, [4,9])
  int x = arr1[7];
}

void some_func(void) {
  global_array[0] = 5;

  // these should be side-effected to the global invariant
  for(int i=0; i < 50; i++) {
    global_array[i] = 42;
  }

  // we currently get top for x here, check if that also happened in the old
  // version
  int x = global_array[0];
}