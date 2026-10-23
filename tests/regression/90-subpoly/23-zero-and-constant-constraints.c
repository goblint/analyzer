// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
Linexpr_managment.add_term
Linexpr_managment.normalize_linexpr
D.assert_constraint
D.is_bot
D.join
ExpressionBounds.bound_texpr
*/

int main(void) {
    int x;
    int y;
    int c;

    __goblint_assume(x - x <= 0);
    __goblint_assume(y - y == 0);

    if (c) {
        __goblint_assume(0 == 1);
        x = 100;
    } else {
        x = 7;
    }

    __goblint_check(x == 7); // SUCCESS
    __goblint_check(y == 0); // UNKNOWN!

    return 0;
}
