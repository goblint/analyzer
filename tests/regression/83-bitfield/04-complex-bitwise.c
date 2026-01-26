// PARAM: --disable ana.int.interval --disable ana.int.def_exc --enable ana.int.bitfield
#include <stdlib.h>
#include <stdio.h>
#include <goblint.h>

int main() {
  int a;
  int b = 21;  // 10101 in binary

  int r = rand() % 2;
  switch (r) {
    case 0:
      a = 19;  // 10011 in binary
      printf("a = 19\n");
      break;
    default:
      a = 17;  // 10001 in binary
      printf("a = 17\n");
      break;
  }

  // AND
  int c_and = a & b;

  if (c_and == 17) {
    __goblint_check(1);  // reachable (19 & 21 = 17, 17 & 21 = 17)
  }
  if (c_and != 17) {
    __goblint_check(0);  // NOWARN (unreachable)
  }

  // OR
  int c_or = a | b;

  if (c_or == 23) {
    __goblint_check(1);  // reachable (19|21 = 23)
  }
  if (c_or == 21) {
    __goblint_check(1);  // reachable (17|21 = 21)
  }
  if (c_or > 23 || c_or < 21) {
    __goblint_check(0);  // NOWARN (unreachable)
  }

  // XOR
  int c_xor = a ^ b;

  if (c_xor == 6) {
    __goblint_check(1);  // reachable (19^21 = 6)
  }
  if (c_xor == 4) {
    __goblint_check(1);  // reachable (17^21 = 4)
  }
  if (c_xor > 6 || c_xor < 4) {
    __goblint_check(0);  // NOWARN (unreachable)
  }

  // Left shift
  int c_lshift = a << 1;

  if (c_lshift == 38) {
    __goblint_check(1);  // reachable (19<<1 = 38)
  }
  if (c_lshift == 34) {
    __goblint_check(1);  // reachable (17<<1 = 34)
  }
  if (c_lshift > 38 || c_lshift < 34) {
    __goblint_check(0);  // NOWARN (unreachable)
  }

  // Right shift
  int c_rshift = a >> 1;

  if (c_rshift == 9) {
    __goblint_check(1);  // reachable (19>>1 = 9)
  }
  if (c_rshift == 8) {
    __goblint_check(1);  // reachable (17>>1 = 8)
  }
  if (c_rshift > 9 || c_rshift < 8) {
    __goblint_check(0);  // NOWARN (unreachable)
  }

  // Check power of two formula
  int a = 16;
  __goblint_assert((a & (a - 1)) == 0);  // SUCCESS

  return 0;
}