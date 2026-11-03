// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assert_constraint
D.assign_exp
ExpressionBounds.bound_texpr
P.propagate

Transitive equality chain: bounds must propagate through several equality
rows, and derived differences must be exact.
*/

int main(void) {
    int x;
    int y;
    int z;
    int w;

    __goblint_assume(x == y);
    __goblint_assume(y == z);
    __goblint_assume(z >= 3);

    __goblint_check(x == z); // SUCCESS
    __goblint_check(x >= 3); // SUCCESS
    __goblint_check(y >= 3); // SUCCESS

    w = x - z;
    __goblint_check(w == 0); // SUCCESS

    return 0;
}
