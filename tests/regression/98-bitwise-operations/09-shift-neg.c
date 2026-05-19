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

  // Possibly negative shift amount: warn
  // (top is uninitialized, representing a non-deterministic value — a common convention in goblint tests)
  if (top) { neg = -1; } else { neg = 1; }
  res = 8 << neg; //WARN
  res = 8 >> neg; //WARN

  // Provably non-negative, bounded interval: no warning
  // (pos_shift is [0,5] — non-negative and within valid shift range)
  int pos_shift = 0;
  if (top) { pos_shift = 5; }
  res = 8 << pos_shift; //NOWARN
  res = 8 >> pos_shift; //NOWARN

  return 0;
}
