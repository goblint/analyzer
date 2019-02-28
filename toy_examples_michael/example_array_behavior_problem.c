#include <stdlib.h>
int global_array[50];

int main(void) {
  some_func();
  
  int x = global_array[5];
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