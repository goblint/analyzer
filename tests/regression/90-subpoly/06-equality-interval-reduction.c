// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
SubPolyDomain.set_interval
ExpressionBounds.bound_texpr
D.assert_constraint
D.assign_exp
*/

int main(void) {
    int x;
    int y;

    __goblint_assume(x - y == 0);
    __goblint_assume(x >= 5);

    __goblint_check(y >= 5); // SUCCESS

    return 0;
}
