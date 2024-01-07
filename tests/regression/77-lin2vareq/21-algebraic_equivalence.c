//SKIP PARAM: --set ana.activated[+] lin2vareq
#include <stdio.h>

int main() {
    int a = 5;
    int b = 3;
    int c = 2;
    int d = 0;

    for (int i = 0; i < c; i++) {
        a += i; 
        b *= 2; 
        d += (a - b);
    }

    int expression1 = a * b + d;
    int expression2 = b * a - (d * -1);

    __goblint_check(expression1 == expression2); //UNKNOWN!

    return 0;
}
