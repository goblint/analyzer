// PARAM: --set ana.activated "['base','threadid','threadflag','escape','mallocWrapper']" --set dbg.debug true --enable ana.arrayoob --enable ana.int.interval

// Variable sized array: oob access

#include <stdio.h>
#include <stdlib.h>
int main() {
  int arr1[] = {1,2,3};
  int top;
  if(top == 0) {
  arr1[top] = 1; // NOWARN
  } else if(top > 2) {
  arr1[top] = 2; // WARN
  } else if (top < 0) {
  arr1[top] = 5; // WARN
  } else {
  arr1[top] = 3; // NOWARN
  }
  int N = 3 + 2;
  int arr[N];
  arr[0] = 1;
  arr[1] = 2;
  arr[2] = 3;
  arr[3] = 4;
  arr[4] = 5;   // NOWARN
  arr[-1] = 10; // WARN
  for (int i = 0; i < 5; ++i) {
    arr[i] = 5; // NOWARN
  }
  for (int i = 0; i <= 5; ++i) {
    arr[i] = 5; // WARN
  }
  for (int i = -2; i < 5; ++i) {
    arr[i] = 5; // WARN
  }
  return 0;
}
