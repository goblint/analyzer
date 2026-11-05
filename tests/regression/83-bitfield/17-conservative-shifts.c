// PARAM: --disable ana.int.interval --disable ana.int.def_exc --enable ana.int.bitfield
#include <goblint.h>
#include <limits.h>

int main(int argc, char **argv) {
  // C11 6.5.7p3: a possibly negative count gives top.
  // The count is signed independently of the unsigned left operand.
  int count = (argc & 1) ? -1 : 1;
  unsigned int left = 1U << count; // NOCRASH
  unsigned int right = 1U >> count; // NOCRASH
  __goblint_check(left <= 2U); // UNKNOWN
  __goblint_check(right <= 1U); // UNKNOWN

  // C11 6.5.7p3: reaching the full width is invalid, not modular shifting.
  count = (argc & 1) ? sizeof(unsigned int) * CHAR_BIT : 0;
  left = 1U << count;
  right = 1U >> count;
  __goblint_check(left <= 1U); // UNKNOWN
  __goblint_check(right <= 1U); // UNKNOWN

  // C11 6.5.7p4: negative signed left operands are undefined even if the
  // mathematical result fits, including a shift by zero.
  int value = -1;
  int result = value << 1;
  __goblint_check(result == -2); // UNKNOWN
  value = (argc & 1) ? -1 : 1;
  result = value << 0;
  __goblint_check(result == value); // UNKNOWN

  // Valid boundary counts include the sign-bit position (6.5.7p3-p5).
  count = sizeof(unsigned int) * CHAR_BIT - 1;
  left = 1U << count;
  right = UINT_MAX >> count;
  __goblint_check(left == UINT_MAX / 2U + 1U);
  __goblint_check(right == 1U);
  value = INT_MAX;
  result = value >> count;
  __goblint_check(result == 0);

  // Promotions precede shifting: unsigned char is not shifted at char width.
  unsigned char small = 1;
  count = sizeof(unsigned char) * CHAR_BIT;
  __goblint_check((small >> count) == 0);
  return 0;
}
