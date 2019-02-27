#include <stdlib.h>
int main(void) {
  test1();
  test2();
  test3();
  test4();
}


void test1(void) {
  int array1[10][10];
  int i=0;

  while(i < 10) {
       // Right now this works because we treat this as an assignment that we do not know by how much the move was. Is it smart to do this for all except +1 or -1?
       //         => Assumption that we only increment/decrement in a loop
      int j=0;
      
      while(j < 10) {
        array1[i][j] = 42;
        j++;
      }

      i++;
  }
}

// More or less interesting results depending on which solver
// is used (and probably on the evluation order of the program points)
//    -> effectWCon (interesting results)
//    -> slr3 (sound but boring top)
void test2(void) {
  int array1[10][10];
  int i=0;

  while(i < 1) {
      int j=0;
      while(j < 10) {
        array1[i][j] = 42;
        j++;
      }

      array1[i][0] = 42;
      i++;
  }
}

void test3(void) {
  int array1[10][10];
  int i = 0;
  while(i < 10) {
    array1[i][0] = 42;
    i++;
  }
}

void test4(void) {
  int array[10][10];
  int i=0;
  while(i < 10) {
    array[i][i] = 42;
    i++;
  }
}