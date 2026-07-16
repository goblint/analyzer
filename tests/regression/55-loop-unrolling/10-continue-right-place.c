// PARAM: --set lib.activated '["goblint"]' --exp.unrolling-factor 11
#include <goblint.h>

int main() {
  int i = 0;
  int j = 10;
  while (i <= 10) {
    if (i == 5) {
      i = 7;
      j = 3;
      continue; // the continue should jump to the following iteration, not all the way to the unrolled loop
    }
    __goblint_check(i + j == 10);
    i++; j--;
  }
  return 0;
}
