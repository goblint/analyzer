// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assign_exp
D.substitute_exp
SubPolyDomain.dim_remove
P.rescue_dependent_slacks

Relations between return values and arguments must survive the call
boundary (return-variable rescue + argument substitution), including when
chained through two calls.
*/

int inc(int a) {
    return a + 1;
}

int main(void) {
    int x;

    int r = inc(x);
    __goblint_check(r == x + 1); // SUCCESS

    int s = inc(r);
    __goblint_check(s == r + 1); // SUCCESS
    __goblint_check(s == x + 2); // SUCCESS

    return 0;
}
