// PARAM: --disable ana.int.interval --disable ana.int.def_exc --enable ana.int.bitfield
#include <goblint.h>

int main(void) {
  int choice;
  int negative = choice ? -1 : -3;

  // C division truncates toward zero, unlike arithmetic right shift.
  int quotient = negative / 2;
  __goblint_check(quotient < 0); // UNKNOWN!
  __goblint_check(quotient == 0); // UNKNOWN!

  // A nonzero remainder has the dividend's sign, unlike low-bit masking.
  int remainder = negative % 2;
  __goblint_check(remainder == -1); // UNKNOWN
  __goblint_check(remainder == 1); // UNKNOWN

  int mixed = choice ? -1 : 1;
  __goblint_check(mixed / 2 == 0); // UNKNOWN
  __goblint_check(mixed % 2 < 0); // UNKNOWN!

  // Division and remainder by one are valid shortcuts for either sign.
  __goblint_check(negative / 1 < 0);
  __goblint_check(negative % 1 == 0);
  __goblint_check(mixed % 1 == 0);

  // Constant operands still use exact C arithmetic.
  int constant = -3;
  __goblint_check(constant / 2 == -1);
  __goblint_check(constant % 2 == -1);

  // Nonnegative and unsigned dividends retain the power-of-two shortcuts.
  int positive = choice ? 5 : 7;
  __goblint_check(positive / 2 >= 2);
  __goblint_check(positive / 2 <= 3);
  __goblint_check(positive % 2 == 1);

  unsigned int unsigned_positive = choice ? 5U : 7U;
  __goblint_check(unsigned_positive / 2U >= 2U);
  __goblint_check(unsigned_positive / 2U <= 3U);
  __goblint_check(unsigned_positive % 2U == 1U);
  return 0;
}
