// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assert_constraint
ExpressionBounds.bound_texpr
SubPolyDomain.add_affeq_row
SubPolyDomain.set_interval
*/

int main(void) {
    int x;
    int y;
    int z;

    __goblint_assume(x == y + 1);
    __goblint_assume(y == z);
    __goblint_assume(x >= 4);
    __goblint_assume(x <= 5);

    __goblint_check(y >= 3); // SUCCESS
    __goblint_check(y <= 4); // SUCCESS
    __goblint_check(z >= 3); // SUCCESS
    __goblint_check(z <= 4); // SUCCESS

    return 0;
}
