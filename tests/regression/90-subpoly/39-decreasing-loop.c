// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assign_exp
D.assert_constraint
SubPolyDomain.widen
SubPolyDomain.leq

Count-down loop: widening must hit the lower bound (not the upper one as in
the count-up tests), the exit guard caps from above, and the relational
invariant i <= n must survive the loop.
*/

int main(void) {
    int n;
    __goblint_assume(n >= 0);

    int i = n;
    while (i > 0) {
        i--;
    }

    __goblint_check(i <= 0); // SUCCESS
    __goblint_check(i >= 0); // SUCCESS
    __goblint_check(i == 0); // SUCCESS
    __goblint_check(i <= n); // SUCCESS

    return 0;
}
