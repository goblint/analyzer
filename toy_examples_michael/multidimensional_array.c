#include <stdlib.h>
int main(void) {
  test1();
  test2();
  test3();
  test4();
  test5();
}


void test1(void) {
  // This test works with both slr3 and new

  int array1[10][10];
  int i=0;
  int j=0;

  while(i < 10) {
      j = 0;
      while(j < 10) {
        array1[i][j] = 42;
        j++;
      }

      // the idea would be to test for covering the entire array on the Neg() [and why not also Pos()] edge
      // N.B. no problem as the entire array is never covered by one segment if it just became partitioned

      i++;
  }
}

void test2(void) {
  int array1[10][10];
  int i = 9;
  int j = 9;

  while(i >= 0){
    j = 9;
    while(j >= 0) {
      array1[i][j] = 42;
      j--;
    }
    i--;
  }
}

// More or less interesting results depending on which solver
// is used (and probably on the evaluation order of the program points)
//    -> effectWCon (interesting results)
//    -> slr3 (sound but boring top)
void test3(void) {
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

void test4(void) {
  int array1[10][10];
  int i = 0;
  while(i < 10) {
    array1[i][0] = 42;
    i++;
  }
}

void test5(void) {
  int array[10][10];
  int i=0;
  while(i < 10) {
    array[i][i] = 42;
    i++;
  }
}