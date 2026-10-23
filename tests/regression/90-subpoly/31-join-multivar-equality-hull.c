// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assert_constraint
D.assign_exp
SubPolyDomain.join
SubPolyDomain.widen
P.affine_hull
ExpressionBounds.bound_texpr

Joins/widens with differing pivot structures: a multi-variable equality
over unbounded variables, shared by both operands, must survive when one
operand has an extra row with an earlier pivot (the shape on which the
library linear_disjunct drops the shared row — subpoly uses its own exact
P.affine_hull). The interval component cannot mask a loss here; the
equality-recovery step in join/widen can, so this test guards the
hull + recovery + restore_defining_rows pipeline as a whole (the exact-hull
property itself is guarded by the QCheck absorption law). The loop repeats
the differing-pivot join under widening.
*/

int main(void) {
    int x;
    int y;
    int z;
    int w;
    int c;
    int n;

    __goblint_assume(y == z + 2);

    if (c) {
        __goblint_assume(x == w - 5);
    }

    __goblint_check(y - z == 2);  // SUCCESS
    __goblint_check(y == z + 2);  // SUCCESS
    __goblint_check(x == w - 5);  // UNKNOWN!

    int k = 0;
    while (k < n) {
        if (k > 2) {
            x = w - 5;
        }
        k++;
    }

    __goblint_check(y - z == 2);  // SUCCESS

    return 0;
}
