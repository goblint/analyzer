// PARAM: --disable ana.int.interval --disable ana.int.def_exc --enable ana.int.bitfield
#include <stdlib.h>
#include <goblint.h>

void basic_join() {
  int a = 8;
  int b = 10;

  int c;
  if (rand()) {
    c = a;
  } else {
    c = b;
  }
  // c should be 0b0000.0000.0000.0000.0000.0000.0000.010?0

  int definite_ones = 8;     // 0b0000.0000.0000.0000.0000.0000.0000.1000
  int definite_zeros = -11;  // 0b1111.1111.1111.1111.1111.1111.1111.0101

  __goblint_assert((c & definite_ones) == definite_ones);     // SUCCESS
  __goblint_assert((~c & definite_zeros) == definite_zeros);  // SUCCESS
}

void join_with_cast() {
  int a = 511;
  char b = 10;

  unsigned char c;
  if (rand()) {
    c = a;
  } else {
    c = b;
  }
  // c should be 0b????.1?1?

  char definite_ones = 10;  // 0b0000.1010
  char definite_zeros = 0;  // 0b0000.0000

  __goblint_assert((c & definite_ones) == definite_ones);     // SUCCESS
  __goblint_assert((~c & definite_zeros) == definite_zeros);  // SUCCESS
}

void join_loop() {
  unsigned char a = 16;

  while (a < 128) {
    a *= 2;
  }
  // a should be 0b????.0000

  char definite_ones = 0;    // 0b0000.0000
  char definite_zeros = 15;  // 0b0000.1111

  __goblint_assert((a & definite_ones) == definite_ones);     // SUCCESS
  __goblint_assert((~a & definite_zeros) == definite_zeros);  // SUCCESS
}

int main() {
  basic_join();
  join_with_cast();
  join_loop();

  return 0;
}
