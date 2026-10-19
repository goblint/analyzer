// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
SubPolyDomain.join
RationalInterval.join
SubPolyDomain.leq
SubPolyDomain.copy
*/

int main(void) {
    int x;
    int y;
    int c;

    if (c) {
        __goblint_assume(x - y <= 0);
    } else {
        __goblint_assume(x - y <= 5);
    }

    __goblint_check(x - y <= 5); // SUCCESS

    return 0;
}
