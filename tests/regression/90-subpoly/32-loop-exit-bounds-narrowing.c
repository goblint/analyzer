// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assign_exp
D.assert_constraint
SubPolyDomain.widen
SubPolyDomain.narrow
ExpressionBounds.bound_texpr

Classic count-up loop: the exit guard gives the lower bound, narrowing after
widening should recover the upper bound.
*/

int main(void) {
    int i = 0;

    while (i < 100) {
        i++;
    }

    __goblint_check(i >= 100); // SUCCESS
    __goblint_check(i <= 100); // SUCCESS
    __goblint_check(i == 100); // SUCCESS

    return 0;
}
