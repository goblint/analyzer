// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.invariant
ExpressionBounds.bound_texpr
D.eval_interval
SubPolyDomain.leq
SubPolyDomain.meet
*/

int main(void) {
    int a;
    int b;

    __goblint_assume(a + b <= 10);
    __goblint_check(a + b <= 10); // SUCCESS

    return 0;
}
