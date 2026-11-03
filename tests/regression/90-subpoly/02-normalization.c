// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none
// NOCRASH

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
Linexpr_managment.linexpr_of_exp
Linexpr_managment.scale_linexpr
Linexpr_managment.normalize_linexpr
Slack_managment.absorb_linexpr_const_into_interval
RationalInterval.add_const
RationalInterval.scale
*/

int main(void) {
    int a;
    int b;

    __goblint_assume(2 * a + 2 * b + 2 < 4);
    __goblint_assume(a + b + 1 < 2);

    return 0;
}
