// PARAM: --enable ana.int.congruence --disable ana.int.def_exc
// This test ensures that operations on constant congr. classes (i.e. classes of the form {k} : arbitrary integer k) yield concrete vals

int main() {
    // basic arithmetic operators
    int a = 1;
    int b = 2;
    int c = -1;
    int d = -2;

    // logical operators
    int zero = 0;
    int one = 1;

    unsigned int uns_z = 0;

    assert ((zero || one) == 1); assert ((zero || zero) == 0); assert ((one || one) == 1);
    assert ((zero && one) == 0); assert ((zero && zero) == 0); assert ((one && one) == 1);
    assert (!one == 0); assert (!zero == 1);

    // bitwise operators
    assert ((zero & zero) == 0); assert ((zero & one) == 0); assert ((one & zero) == 0); assert ((one & one) == 1);
    assert ((zero | zero) == 0); assert ((zero | one) == 1); assert ((one | zero) == 1); assert ((one | one) == 1);
    assert ((zero ^ zero) == 0); assert ((zero ^ one) == 1); assert ((one ^ zero) == 1); assert ((one ^ one) == 0);

    // shift-left
    // TODO Implement shift-left
    // unsigned char m = 136;
    // assert ((m << 1) == 16);

    //shift-right missing as only top() is returned currently

    // comparisons
    assert ((a < b) == 1);
    assert ((a > b) == 0);
    assert ((a == b) == 0);
    assert ((a != b) == 1);

    return 0;
}