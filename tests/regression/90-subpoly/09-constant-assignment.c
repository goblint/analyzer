// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assign_exp
Slack_managment.add_constant_interval
SubPolyDomain.set_interval
ExpressionBounds.bound_texpr
*/

int main(void) {
    int x = 4;
    int y;

    __goblint_assume(x + y <= 10);

    __goblint_check(y <= 6); // SUCCESS

    return 0;
}
