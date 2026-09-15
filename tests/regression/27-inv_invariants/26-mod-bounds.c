// PARAM: --enable ana.int.interval
// Refining the interval of a in "a % b == c" must only move the bounds of a to values
// that are congruent to c modulo b, otherwise satisfying values are cut away.
#include <goblint.h>

int main() {
  int x;
  if (x > 1 && x < 5 && x % 2 == 1) // x = [2,4] && x % 2 == 1 => x = 3
    __goblint_check(x == 3);

  int y;
  if (y >= -4 && y <= -2 && y % 2 == -1) // y = [-4,-2] && y % 2 == -1 => y = -3
    __goblint_check(y == -3);

  // The upper bound 3 is divisible by 3, so it cannot be attained, but 2 still can.
  // Used to be considered dead because the bound was moved to 3-2=1 instead of 3-(3-2)=2.
  int z;
  if (z >= 0 && z <= 3 && z % 3 == 2)
    __goblint_check(z == 2);

  // Same for a negative divisor, where only the magnitude of b matters.
  int n;
  if (n >= -28 && n <= -22 && n % -7 == -5)
    __goblint_check(n == -26);

  // Test to show both refinements are needed. The congruence shift accepts 1,
  // but the original mod refinement restricts the sign to negative, so <= -2.
  int s;
  if (s >= -12 && s <= 1 && s % 3 == -2) { // s = -11, -8, -5, -2
    __goblint_check(s >= -11);
    __goblint_check(s <= -2);
  }

  int w;
  if (w >= 0 && w <= 48 && w % 3 == 1) { // w = 1, 4, ..., 46
    __goblint_check(w >= 1);
    __goblint_check(w <= 46);
  }

  int v;
  if (v >= 0 && v <= 48 && v % 3 == 0) { // bounds are already congruent, must not move
    __goblint_check(v >= 0);
    __goblint_check(v <= 48);
  }

  return 0;
}
