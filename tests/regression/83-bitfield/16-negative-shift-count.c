// PARAM: --disable ana.int.interval --disable ana.int.def_exc --enable ana.int.bitfield
#include <goblint.h>

int main(void) {
  int count = -1;

  // The count is signed even though the left operand is unsigned (#2276).
  unsigned long left = 1UL << count; // NOCRASH
  __goblint_check(left == 0); // UNKNOWN
  unsigned long right = 1UL >> count; // NOCRASH
  __goblint_check(right == 0); // UNKNOWN

  unsigned int left_int = 1U << count; // NOCRASH
  __goblint_check(left_int == 0); // UNKNOWN
  unsigned int right_int = 1U >> count; // NOCRASH
  __goblint_check(right_int == 0); // UNKNOWN

  // Valid counts still give precise results.
  count = 0;
  __goblint_check((1UL << count) == 1UL);
  __goblint_check((1UL >> count) == 1UL);
  count = 1;
  __goblint_check((1UL << count) == 2UL);
  __goblint_check((1UL >> count) == 0UL);
  return 0;
}
