// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
Linexpr_managment.normalize_linexpr
Linexpr_managment.scale_linexpr
RationalInterval.scale
D.assert_constraint
ExpressionBounds.bound_texpr
*/

int main(void) {
    int x;
    int y;

    __goblint_assume(-2 * x + 4 * y <= -6);
    __goblint_assume(y == 1);

    __goblint_check(x >= 5); // SUCCESS
    __goblint_check(x >= 6); // UNKNOWN!

    return 0;
}
