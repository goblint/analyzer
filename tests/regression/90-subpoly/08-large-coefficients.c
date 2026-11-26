// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assert_constraint
ExpressionBounds.bound_texpr
Linexpr_managment.linexpr_of_exp
Linexpr_managment.scale_linexpr
Linexpr_managment.normalize_linexpr
*/

int main(void) {
    int x;
    int y;
    int z;

    __goblint_assume(x == 1000 * y - 3 * z);
    __goblint_assume(y == 2);
    __goblint_assume(z == 5);

    __goblint_check(x == 1985); // SUCCESS

    return 0;
}
