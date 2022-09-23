//SKIP PARAM: --set ana.activated[+] affeq  --set sem.int.signed_overflow assume_none

int main() {
    int a, b ,c, d;

    int x = 4 * a + 3 * b + 2 * c + 1 * d;

    __goblint_check(2 * x == 8 * a + 6 * b + 4 * c + 2 * d);

    int y = 2 * 100 + 3 * c + 400 - 100;

    __goblint_check(y == 3 * c + 500);

    int z = 5;

    z = a * 2 + c * (1 * (5 - d)) + z;

    __goblint_check(z == 5); //UNKNOWN!

    int a = 2;
    int b = 3;
    int c = 1;

    int x = a * a + c / c;
    __goblint_check(x == 5);
}
