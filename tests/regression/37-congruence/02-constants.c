// PARAM: --enable ana.int.congruence --set sem.int.signed_overflow assume_none --disable ana.int.def_exc
// This test ensures that operations on singleton congr. classes (i.e. classes of the form {k} : arbitrary integer k) yield precise vals
#include <assert.h>

int main() {
    // basic arithmetic operators
    int a = 1;
    int b = 2;
    int c = -1;
    int d = -2;

    __goblint_check(a + b == 3); __goblint_check(a + d == -1);
    __goblint_check(a * b == 2); __goblint_check(b * c == -2);
    __goblint_check(a / b == 0); __goblint_check(d / c == 2);
    __goblint_check(b % a == 0); __goblint_check(d % c == 0);
    __goblint_check(-a == -1); __goblint_check(-d == 2);

    // logical operators
    int zero = 0;
    int one = 1;

    unsigned int uns_z = 0;

    //arithmetic operations

    __goblint_check((zero || one) == 1); __goblint_check((zero || zero) == 0); __goblint_check((one || one) == 1);
    __goblint_check((zero && one) == 0); __goblint_check((zero && zero) == 0); __goblint_check((one && one) == 1);
    __goblint_check(!one == 0); __goblint_check(!zero == 1);

    // bitwise operators
    __goblint_check((zero & zero) == 0); __goblint_check((zero & one) == 0); __goblint_check((one & zero) == 0); __goblint_check((one & one) == 1);
    __goblint_check((zero | zero) == 0); __goblint_check((zero | one) == 1); __goblint_check((one | zero) == 1); __goblint_check((one | one) == 1);
    __goblint_check((zero ^ zero) == 0); __goblint_check((zero ^ one) == 1); __goblint_check((one ^ zero) == 1); __goblint_check((one ^ one) == 0);

    // comparisons
    __goblint_check((a < b) == 1);
    __goblint_check((a > b) == 0);
    __goblint_check((a == b) == 0);
    __goblint_check((a != b) == 1);

    return 0;
}
