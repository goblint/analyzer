// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assert_constraint
D.assign_exp
D.is_bot
D.join
SubPolyDomain.meet
SubPolyDomain.join
*/

int main(void) {
    int x;
    int c;

    if (c) {
        __goblint_assume(x == 0);
        __goblint_assume(x == 1);
    } else {
        x = 5;
    }

    __goblint_check(x == 5); // SUCCESS

    return 0;
}
