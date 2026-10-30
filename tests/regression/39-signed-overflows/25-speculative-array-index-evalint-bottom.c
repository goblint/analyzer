// PARAM: --enable ana.int.interval --set ana.base.arrays.domain partitioned --set ana.base.partition-arrays.keep-expr last --set sem.int.signed_overflow assume_none

int main(void) {
  int a[129];
  signed char i = 127;

  a[i] = 1;
  a[i + 1] = 2;

  // Both array writes are in bounds. The partitioned-array domain nevertheless
  // speculatively evaluates the synthetic signed-char expression i + 1.
  __goblint_check(1); // SUCCESS
  return 0;
}
