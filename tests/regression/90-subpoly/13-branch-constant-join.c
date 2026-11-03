// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assign_exp
ExpressionBounds.bound_texpr
RationalInterval.join
SubPolyDomain.join
*/

int main(void) {
    int x;
    int c;

    if (c) {
        x = 0;
    } else {
        x = 1;
    }

    __goblint_check(x >= 0); // SUCCESS
    __goblint_check(x <= 1); // SUCCESS
    __goblint_check(x == 0); // UNKNOWN!

    return 0;
}
