//SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none
#include <stdio.h>

int main() {
    int a = 5;
    int b = 3;
    int c = 2;
    int d = 1;

    for (int i = 0; i < 3; i++) {
        a += d;
        b += a;
        c += b;
        d *= 2;
    }

    int expression1 = a * (b + c);
    int expression2 = (a * b) + (a * c);

    __goblint_check(expression1 == expression2); //UNKNOWN!

    return 0;
}
