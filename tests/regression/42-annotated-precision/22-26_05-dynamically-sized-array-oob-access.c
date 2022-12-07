// PARAM: --enable ana.arrayoob --enable annotation.int.enabled --set ana.int.refinement fixpoint

// Variable sized array: oob access

int main() __attribute__((goblint_precision("no-def_exc","interval")));

#include <stdio.h>
#include <stdlib.h>
int main() {
  int top;
  int N;
// The if statement is needed, so the size is actually dynamic
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
