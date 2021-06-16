// PARAM: --set ana.activated "['base','threadid','threadflag','escape','mallocWrapper']" --set dbg.debug true --enable ana.arrayoob --enable ana.int.interval --enable ana.int.interval --enable ana.int.enums

// Variable sized array: oob access

#include <stdio.h>
#include <stdlib.h>
int main() {
  int top;
  int N;

  if (top) {
    N = 5;
  } else {
    N = 10;
  }
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
