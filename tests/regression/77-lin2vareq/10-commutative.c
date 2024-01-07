//SKIP PARAM: --set ana.activated[+] lin2vareq
#include <stdio.h>

int main() {
    int a = 5;
    int b = 3;

    int expression1 = a * b;

    int expression2 = b * a;

    __goblint_check(expression1 == expression2); //SUCCESS

    return 0;
}

//This test case checks the commutative property.
