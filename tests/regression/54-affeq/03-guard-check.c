//SKIP PARAM:  --set ana.activated[+] affeq --set ana.apron.privatization dummy --set sem.int.signed_overflow assume_none
void main(void) {

    int i = 0;
    int k = 0;
    int z = i + k;

    int x, y;
    int f = x + y;

    //EQ
    if (x == 0) {
        assert (x == 0);
    }
    assert (2 * z == 2 * i + 2 * k);
    assert (2 * f == 2 * x + 2 * y);

    //DISEQ
    int p = 0;
    if (i != 1) {
        assert (i != 1);
        p = 1;
    } else {
        p = 1000;
    }
    assert (p == 1);

    //SUP
    assert (f > x + y); //FAIL
    assert (z > i + k); //FAIL

    //SUPEQ
    assert (f < x + y); //FAIL
    assert (f >= x + y);
    assert (z >= i + k + 5); //FAIL

}
