//SKIP PARAM: --set ana.activated[+] affeq  --set sem.int.signed_overflow assume_none

int main() {
    int a, b ,c, d;

    int x = 4 * a + 3 * b + 2 * c + 1 * d;

    assert (2 * x == 8 * a + 6 * b + 4 * c + 2 * d);

    int y = 2 * 100 + 3 * c + 400 - 100;

    assert (y == 3 * c + 500);

    int z = 5;

    z = a * 2 + c * (1 * (5 - d)) + z;

    assert (z == 5); //UNKNOWN!
}
