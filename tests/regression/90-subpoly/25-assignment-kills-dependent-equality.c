// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assign_exp
D.forget_var
D.rem_rows_containing_var
D.rem_slacks_containing_var
SubPolyDomain.dim_remove
ExpressionBounds.bound_texpr
*/

int main(void) {
    int x;
    int y;
    int z;

    __goblint_assume(x == y + 2);
    __goblint_assume(y >= 0);

    __goblint_check(x >= 2); // SUCCESS

    y = z;

    __goblint_check(x == y + 2); // UNKNOWN!
    __goblint_check(x >= 2);     // SUCCESS (x unchanged; reduction keeps its bound when y is havocked)

    return 0;
}
