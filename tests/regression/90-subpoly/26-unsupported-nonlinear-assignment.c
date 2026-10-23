// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
Linexpr_managment.linexpr_of_exp
D.assign_exp
D.forget_var
D.rem_rows_containing_var
D.rem_slacks_containing_var
ExpressionBounds.bound_texpr
*/

int main(void) {
    int x;
    int y;
    int z;

    __goblint_assume(x <= y);
    __goblint_assume(y <= 10);

    x = x * z;

    __goblint_check(x <= y);  // UNKNOWN!
    __goblint_check(y <= 10); // SUCCESS

    return 0;
}
