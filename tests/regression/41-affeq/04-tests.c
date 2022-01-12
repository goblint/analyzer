// PARAM: --enable ana.int.congruence --set sem.int.signed_overflow assume_none --disable ana.int.def_exc
// This test ensures that operations on singleton congr. classes (i.e. classes of the form {k} : arbitrary integer k) yield precise vals

int main() {
    // basic arithmetic operators
    int a = 1;
    int b = 2;

    assert (a + b == 3);

    return 0;
}
