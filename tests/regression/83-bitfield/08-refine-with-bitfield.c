// PARAM: --enable ana.int.bitfield --set ana.int.refinement fixpoint
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int main() {
  int a = rand();

  // Basic bitwise properties
  __goblint_assert((a & 0) == 0);                    // Any number ANDed with 0 is 0
  __goblint_assert((a | 0xFFFFFFFF) == 0xFFFFFFFF);  // Any number ORed with all 1s gives all 1s

  // Testing alignment and divisibility with powers of 2
  int ALIGN_8 = 0x7;  // 111 in binary
  if ((a & ALIGN_8) == 0) {
    __goblint_assert(a % 8 == 0);  // Number is aligned to 8
  }

  int ALIGN_32 = 0x1F;  // 11111 in binary
  if ((a & ALIGN_32) == 0) {
    __goblint_assert(a % 32 == 0);  // Number is divisible by 32
  }

  // Testing specific power of 2 patterns
  int POW2_MASK = (1 << 4) - 1;  // 15 (0b1111)
  if ((a & POW2_MASK) == 8) {
    __goblint_assert((a & 0xf) == 8);  // Exactly bit 3 set in lower 4 bits
    __goblint_assert((a & 12) == 8);   // Bits 2-3 must be 1000
    __goblint_assert((a & 3) == 0);    // Bits 0-1 must be 0
  }

  // Testing specific bit patterns and masking
  if ((a & 0x3) == 0x3) {
    __goblint_assert(a % 4 >= 3);    // Last two bits are 1
    __goblint_assert((a & 1) == 1);  // Least significant bit must be 1
  }

  if ((a & 0xC) == 0x8) {                // 1000 in binary
    __goblint_assert((a & 0x4) == 0);    // Bit 2 must be 0
    __goblint_assert((a & 0x8) == 0x8);  // Bit 3 must be 1
  }

  // Testing OR operations with patterns
  int OR_MASK = 0x55;  // 01010101 in binary
  if ((a | OR_MASK) == 0x55) {
    __goblint_assert((a | 0xFF) == 0xFF);  // ORing with all 1s gives all 1s
  }

  if ((a | 0x6) == a) {
    __goblint_assert((a & 0x6) == 0x6);  // Bits 1 and 2 must be set
  }

  // Testing XOR operations
  int XOR_MASK = 0xAA;  // 10101010 in binary
  if ((a ^ XOR_MASK) == 0) {
    __goblint_assert(a == 0xAA);           // Must match the mask exactly
    __goblint_assert((a & 0xAA) == 0xAA);  // All alternating bits must be 1
  }

  if ((a ^ 0xFF) == 0) {
    __goblint_assert(a == 0xFF);  // Only possible if a is 0xFF
  }

  // Testing complex bit patterns
  int COMPLEX_MASK = 0x33;  // 00110011 in binary
  if ((a & COMPLEX_MASK) == 0x11) {
    __goblint_assert((a & 0x22) == 0);     // Middle bits must be 0
    __goblint_assert((a & 0x11) == 0x11);  // Outer bits must be 1
  }

  // Testing shifted masks and patterns
  int SHIFT_MASK = 3 << 2;  // 1100 in binary
  if ((a & SHIFT_MASK) == SHIFT_MASK) {
    __goblint_assert((a & 12) == 12);         // Both bits must be set
    __goblint_assert(((a >> 2) & 3) == 3);    // When shifted right, lowest bits must be 11
  }

  if (a == SHIFT_MASK) {
    __goblint_assert(((a << 2) & 48) == 48);  // When shifted left, highest bits must be 11
  }

  int SHIFTED = 0x7 << 3;  // 111000 in binary
  if ((a & SHIFTED) == 0) {
    __goblint_assert((a & 0x38) == 0);  // Bits 3,4,5 must be 0
  }

  // Testing sign bits and negative numbers
  if ((a & 0x80) == 0x80) {
    __goblint_assert(a & 0x80);            // Highest bit must be set
    __goblint_assert((a | 0x7F) >= 0x80);  // Result must be >= 128
  }

  // Testing bitwise complement
  int COMP_MASK = ~0x0F;
  if ((a & COMP_MASK) == 0x0F) {
    __goblint_check(0);  // NOWARN (unreachable)
  }

  return 0;
}