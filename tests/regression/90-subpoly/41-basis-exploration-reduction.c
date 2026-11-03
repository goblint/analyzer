// PARAM: --set ana.activated[+] subpoly --enable ana.subpoly.basis-exploration --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assert_constraint
D.reduce_intervals
ExpressionBounds.bound_texpr
P.explore_linear
P.reduce

Paper Example 5 shape: two equalities over four variables where only two
variables are bounded, and the bounds of the other two exist only through
linear combinations of the rows (half-sum and half-difference). Here the
rref happens to contain the combined row x + y + 2z = 1, so z's bounds and
the refutation-based checks on w succeed even without exploration; what
plain propagation cannot do is *materialize* w's bounds (w = (1 - x + y)/2
in [-1/2, 2], from the basis {z, w} of the linear explorer) into the
interval component. The checks after x is havocked distinguish this: the
row combination through x is gone, and only the interval stored by the
basis-exploration reduction keeps w in [0, 2].
*/

extern int mystery(int v);

int main(void) {
    int w;
    int x;
    int y;
    int z;

    __goblint_assume(x >= 0);
    __goblint_assume(x <= 2);
    __goblint_assume(y >= 0);
    __goblint_assume(y <= 3);

    __goblint_assume(x + z + w == 1);
    __goblint_assume(y + z - w == 0);

    __goblint_check(z >= -2);  // SUCCESS
    __goblint_check(z <= 0);   // SUCCESS
    __goblint_check(z >= -1);  // UNKNOWN!

    __goblint_check(w >= 0);   // SUCCESS
    __goblint_check(w <= 2);   // SUCCESS
    __goblint_check(w <= 1);   // UNKNOWN!

    x = mystery(x);

    // needs the interval on w materialized by basis exploration
    __goblint_check(w >= 0);   // SUCCESS
    __goblint_check(w <= 2);   // SUCCESS
    __goblint_check(w <= 1);   // UNKNOWN!

    return 0;
}
