// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assign_var
D.assign_exp
SubPolyDomain.join
SubPolyDomain.widen
D.assert_constraint
ExpressionBounds.bound_texpr
SubPolyDomain.leq
*/

int main(void) {
    int i;
    int j;
    int x = i;
    int y = j;

    if (x <= 0) {
        return 0;
    }

    while (x > 0) {
        x--;
        y--;
    }

    if (y == 0) {
        __goblint_check(i == j); // SUCCESS
    }

    return 0;
}
