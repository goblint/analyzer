// PARAM: --enable ana.int.congruence --enable ana.int.congruence_no_overflow --disable ana.int.def_exc
// This test ensures that operations on singleton congr. classes (i.e. classes of the form {k} : arbitrary integer k) yield concrete vals

int main() {
    // basic arithmetic operators
    int a = 1;
    int b = 2;
    int c = -1;
    int d = -2;

    assert (a + b == 3); assert (a + d == -1);
    assert (a * b == 2); assert (b * c == -2);
    assert (a / b == 0); assert (d / c == 2);
    assert (b % a == 0); assert (d % c == 0);
    assert (-a == -1); assert (-d == 2);

    // logical operators
    int zero = 0;
    int one = 1;

    unsigned int uns_z = 0;

    //arithmetic operations

    assert ((zero || one) == 1); assert ((zero || zero) == 0); assert ((one || one) == 1);
    assert ((zero && one) == 0); assert ((zero && zero) == 0); assert ((one && one) == 1);
    assert (!one == 0); assert (!zero == 1);

    // bitwise operators
    assert ((zero & zero) == 0); assert ((zero & one) == 0); assert ((one & zero) == 0); assert ((one & one) == 1);
    assert ((zero | zero) == 0); assert ((zero | one) == 1); assert ((one | zero) == 1); assert ((one | one) == 1);
    assert ((zero ^ zero) == 0); assert ((zero ^ one) == 1); assert ((one ^ zero) == 1); assert ((one ^ one) == 0);

    // comparisons
    assert ((a < b) == 1);
    assert ((a > b) == 0);
    assert ((a == b) == 0);
    assert ((a != b) == 1);

    return 0;
}
