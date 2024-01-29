// PARAM: --set ana.activated[+] lin2vareq
#include <stdio.h>

int x = 42;
long y;
short z;

int main() {
    
    y = (long)x;
    z = (short)x;

    int a = (int)y;
    short b = (short)y;

    int c = (int)z;
    long d = (long)z;

    unsigned int u1 = (unsigned int)x;
    unsigned long u2 = (unsigned long)y;
    unsigned short u3 = (unsigned short)z;

    __goblint_check(x == a);
    __goblint_check(x == c);
    __goblint_check(y == d);
    __goblint_check(x == (int)u1);
    __goblint_check(y == (long)u2);
    __goblint_check(z == (short)u3);

    return 0;
}
