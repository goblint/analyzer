// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.forget_var
D.forget_vars
D.rem_rows_containing_var
D.rem_slacks_containing_var
SubPolyDomain.dim_remove
D.substitute_exp
D.assign_exp
*/

int main(void) {
    int x;
    int y;

    __goblint_assume(x + y <= 10);

    x = 100;

    __goblint_check(y <= 10); // UNKNOWN!

    return 0;
}
