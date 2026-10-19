// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assign_exp
D.forget_var
D.rem_rows_containing_var
D.rem_slacks_containing_var
SubPolyDomain.dim_remove
*/

int main(void) {
    int x;
    int y;
    int z;

    __goblint_assume(x <= y);

    x = z;

    __goblint_check(x <= y); // UNKNOWN!

    return 0;
}
