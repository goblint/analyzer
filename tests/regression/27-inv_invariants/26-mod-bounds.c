// PARAM: --enable ana.int.interval
#include <goblint.h>

int main() {
  // Both bounds move inwards to the single odd value between them.
  int x;
  if (x > 1 && x < 5 && x % 2 == 1) // x = [2,4]
    __goblint_check(x == 3);

  // The same with a negative dividend.
  int y;
  if (y >= -4 && y <= -2 && y % 2 == -1) // y = [-4,-2]
    __goblint_check(y == -3);

  // Refine upper bound to 2, the largest value below it congruent to 2 modulo 3.
  int z;
  if (z >= 0 && z <= 3 && z % 3 == 2)
    __goblint_check(z == 2);

  // A negative divisor.
  int n;
  if (n >= -28 && n <= -22 && n % -7 == -5) // n = -26, since -26 = 3 * -7 + -5
    __goblint_check(n == -26);

  // The two bounds move by different amounts: 0 up to 1 and 48 down to 46.
  int w;
  if (w >= 0 && w <= 48 && w % 3 == 1) { // w = 1, 4, ..., 46
    __goblint_check(w >= 1);
    __goblint_check(w <= 46);
  }

  // Both bounds are already congruent to 0, so neither may move.
  int v;
  if (v >= 0 && v <= 48 && v % 3 == 0) {
    if (v == 0)
      __goblint_check(1); // reachable
    if (v == 48)
      __goblint_check(1); // reachable
  }

  // Some of these tests are AI-generated mutation tests.
  // This one shows why we need sign refinement within the congruence-based refinement.
  // The congruence alone would leave the bound at 10, which is also congruent to -2 modulo 3.
  int a;
  if (a >= -12 && a <= 12 && a % 3 == -2) { // a = -11, -8, -5, -2
    __goblint_check(a >= -11);
    __goblint_check(a <= -2);
  }

  // The sign bound also applies when the remainder is non-definite.
  int p;
  if (p >= -20 && p <= 20 && p % 4 > 1) // p % 4 is 2 or 3, so p is positive
    __goblint_check(p >= 2);

  int r;
  if (r >= -20 && r <= 20 && r % 4 < -1) // r % 4 is -2 or -3, so r is negative
    __goblint_check(r <= -2);

  // Here, the remainder is the range [1,2], and our imprecise fallback can still
  // refine 7 -> 6 since 7 % 4 == 3 is not allowed.
  int q;
  if (q >= 0 && q <= 7 && q % 4 > 0 && q % 4 < 3) // q = 1, 2, 5, 6
    __goblint_check(q <= 6);

  int e;
  if (e >= -7 && e <= 0 && e % 4 < 0 && e % 4 > -3) // e = -1, -2, -5, -6
    __goblint_check(e >= -6);

  // A positive remainder bounds m from below even though b is negative.
  int m;
  if (m >= -30 && m <= 30 && m % -7 == 3) { // m = 3, 10, 17, 24
    __goblint_check(m >= 3);
    __goblint_check(m <= 24);
  }

  return 0;
}
