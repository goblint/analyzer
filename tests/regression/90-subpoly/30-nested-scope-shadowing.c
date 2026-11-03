// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assign_exp
D.forget_vars
VarManagement.dim_add
SubPolyDomain.dim_remove
ExpressionBounds.bound_texpr
*/

int main(void) {
    int x = 4;
    int y = 1;

    {
        int x = y + 10;
        __goblint_check(x == 11); // SUCCESS
        x = x + 1;
        __goblint_check(x == 12); // SUCCESS
    }

    __goblint_check(x == 4); // SUCCESS
    __goblint_check(y == 1); // SUCCESS

    return 0;
}
