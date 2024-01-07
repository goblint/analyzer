//SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none
#include <stdio.h>

int main() {
    int a = 5;
    int b = 3;
    int c = 2;

    for (int i = 0; i < c; i++) {
        a += b;
        b += 1;
    }

    int expression1 = a * b;
    int expression2 = b * a;

    __goblint_check(expression1 == expression2); //UNKNOWN

    return 0;
}
