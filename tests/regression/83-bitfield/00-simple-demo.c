// PARAM: --enable ana.int.bitfield
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <goblint.h>

#define ANY_ERROR 5  // 0b0101

int main() {
  int testvar = 235;

  int state;
  int r = rand() % 3;
  switch (r) {
    case 0:
      state = 0; /* 0b000 */
      testvar = 1;
      break;
    case 1:
      state = 8; /* 0b1000 */
      testvar = 1;
      break;
    default:
      state = 10; /* 0b1010 */
      testvar = 1;
      break;
  }

  __goblint_check((state & ANY_ERROR) == 0);
}