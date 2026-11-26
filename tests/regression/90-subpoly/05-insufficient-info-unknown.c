// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assert_constraint
D.invariant
ExpressionBounds.bound_texpr
*/

int main(void) {
    int x;

    __goblint_assume(x >= 0);

    __goblint_check(x >= 0);  // SUCCESS
    __goblint_check(x >= 10); // UNKNOWN!

    return 0;
}
