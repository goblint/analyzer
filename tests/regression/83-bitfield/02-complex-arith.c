// PARAM: --disable ana.int.interval --disable ana.int.def_exc --enable ana.int.bitfield
#include <stdlib.h>
#include <goblint.h>

int main() {
  int a;
  int b = 23;

  int r = rand() % 2;
  switch (r) {
    case 0:
      a = 19;
      printf("a = 19\n");
      break;
    default:
      a = 17;
      printf("a = 17\n");
      break;
  }

  // PLUS

  int c_add = a + b;

  if (c_add == 40) {
    __goblint_check(1);  // reachable
  }
  if (c_add == 42) {
    __goblint_check(1);  // reachable
  }
  if (c_add > 42 || c_add < 40) {
    __goblint_check(0);  // NOWARN (unreachable)
  }

  // MINUS

  int c_minus = b - a;

  if (c_minus == 6) {
    __goblint_check(1);  // reachable
  }
  if (c_minus == 4) {
    __goblint_check(1);  // reachable
  }
  if (c_minus > 6 || c_minus < 4) {
    __goblint_check(0);  // NOWARN (unreachable)
  }

  // MULT

  int c_mult = a * b;

  if (c_mult == 391) {
    __goblint_check(1);  // reachable
  }
  if (c_mult == 437) {
    __goblint_check(1);  // reachable
  }

  // DIV

  // Div on non-unique bitfields is not supported
}