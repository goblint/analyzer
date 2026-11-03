// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assert_constraint
Slack_managment.interval_of_constraint_op
Slack_managment.absorb_linexpr_const_into_interval
RationalInterval.add_const
ExpressionBounds.bound_texpr
*/

int main(void) {
    int x;
    int y;

    __goblint_assume(x + y < 10);
    __goblint_assume(x - y > -3);

    __goblint_check(x + y <= 9);  // SUCCESS
    __goblint_check(x + y <= 8);  // UNKNOWN!
    __goblint_check(x - y >= -2); // SUCCESS
    __goblint_check(x - y >= -1); // UNKNOWN!

    return 0;
}
