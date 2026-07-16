// PARAM: --enable ana.int.bitfield --enable annotation.int.enabled
#include <stdlib.h>
#include <goblint.h>

#define ANY_ERROR 5  // 0b0101
void example1(void) __attribute__((goblint_precision("no-bitfield")));
void example2(void) __attribute__((goblint_precision("bitfield")));

int main() {
  example1();
  example2();
}

void example1() {
  int state;
  int r = rand() % 3;
  switch (r) {
    case 0:
      state = 0; /* 0b0000 */
      break;
    case 1:
      state = 8; /* 0b1000 */
      break;
    default:
      state = 10; /* 0b1010 */
      break;
  }

  __goblint_check((state & ANY_ERROR) == 0);  // UNKNOWN
}

void example2() {
  int state;
  int r = rand() % 3;
  switch (r) {
    case 0:
      state = 0; /* 0b0000 */
      break;
    case 1:
      state = 8; /* 0b1000 */
      break;
    default:
      state = 10; /* 0b1010 */
      break;
  }

  __goblint_check((state & ANY_ERROR) == 0);  // SUCCESS
}