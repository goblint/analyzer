// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
SubPolyDomain.meet
SubPolyDomain.set_slack
SubPolyDomain.mem_slack
RationalInterval.meet
ExpressionBounds.bound_texpr
*/

int main(void) {
    int x;
    int y;

    __goblint_assume(x - y <= 10);
    __goblint_assume(x - y <= 3);
    __goblint_assume(x - y >= -4);
    __goblint_assume(x - y >= -1);

    __goblint_check(x - y <= 3);  // SUCCESS
    __goblint_check(x - y <= 2);  // UNKNOWN!
    __goblint_check(x - y >= -1); // SUCCESS
    __goblint_check(x - y >= 0);  // UNKNOWN!

    return 0;
}
