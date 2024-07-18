// PARAM: --enable ana.int.interval --set solver td3 --enable solvers.td3.remove-wpoint
// Example from "The Top-Down Solver — An Exercise in A²I", Section 6.
#include <goblint.h>

int main() {
  int i, j, x;
  i = 0;
  while (i < 42) {
    j = 0;
    while (j < 17) {
      x = i + j;
      j++;
    }
    __goblint_check(j == 17);
    __goblint_check(i >= 0);
    __goblint_check(i <= 41);
    i++;
  }
  __goblint_check(i == 42);
  __goblint_check(j == 17); // TODO
  return 0;
}
