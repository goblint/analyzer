// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assign_var
D.assign_exp
ExpressionBounds.bound_texpr
Slack_managment.add_constant_interval
SubPolyDomain.set_interval
*/

int main(void) {
    int y;
    int z;
    int x;
    int copy;

    __goblint_assume(y >= 1);
    __goblint_assume(y <= 3);
    __goblint_assume(z >= -4);
    __goblint_assume(z <= -2);

    copy = y;
    x = 2 * y - z + 1;

    __goblint_check(copy >= 1); // SUCCESS
    __goblint_check(copy <= 3); // SUCCESS
    __goblint_check(x >= 5);    // SUCCESS
    __goblint_check(x <= 11);   // SUCCESS

    return 0;
}
