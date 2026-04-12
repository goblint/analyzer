// PARAM: --enable ana.int.interval
#include <goblint.h>

int main() {
  int res;
  int top;
  int neg;

  // Definitely negative shift amount: error
  res = 8 << -2; //WARN

  // Definitely negative shift amount in right shift: error
  res = 8 >> -1; //WARN

  // Non-negative shift amount: no warning
  res = 8 << 2; //NOWARN
  res = 8 >> 2; //NOWARN

  // Zero shift amount: no warning
  res = 8 << 0; //NOWARN
  res = 8 >> 0; //NOWARN

  // Possibly negative shift amount (top): warn
  if (top) { neg = -1; } else { neg = 1; }
  res = 8 << neg; //WARN
  res = 8 >> neg; //WARN

  // Definitely non-negative interval: no warning
  int nonneg;
  if (nonneg < 0) { nonneg = 0; }
  res = 8 << nonneg; //NOWARN
  res = 8 >> nonneg; //NOWARN

  return 0;
}
