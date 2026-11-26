// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
SubPolyDomain.join
SubPolyDomain.leq
SubPolyDomain.set_slack
SubPolyDomain.add_affeq_row
RationalInterval.join
ExpressionBounds.bound_texpr
*/

int main(void) {
    int x;
    int y;
    int z;
    int c;

    if (c) {
        __goblint_assume(x - y <= 2);
        __goblint_assume(z == x + 1);
    } else {
        __goblint_assume(x - y <= 5);
        __goblint_assume(z == y + 1);
    }

    __goblint_check(x - y <= 5); // SUCCESS
    __goblint_check(x - y <= 2); // UNKNOWN!
    __goblint_check(z == x + 1); // UNKNOWN!
    __goblint_check(z == y + 1); // UNKNOWN!

    return 0;
}
