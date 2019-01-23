#include <stdlib.h>
int main(void) {
  int array1[10][10];

  int i=0;
  int j=0;

  while(i < 10) {
      while(j < 10) {
        array1[i][j] = 42;
        j++;
      }
    i++;
  }

  // Current (unsound) result 
  // array1 -> (Array (partitioned by i):(
  //              (Array (partitioned by i): (Uninitialized -- [42,42] -- Unknown), 10) -- (* not ok, would need to be partitioned by j *)
  //              (Array (partitioned by Bot): (Uninitialized -- Uninitialized -- Uninitialized), 10) -- (* ok *)
  //              (Array (partitioned by Bot): (Uninitialized -- Uninitialized -- Uninitialized), 10))   (* ok *)
  //              , 10)


}