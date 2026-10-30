// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
SubPolyDomain.join
SubPolyDomain.add_affeq_row
SubPolyDomain.set_slack
RationalInterval.join
*/

int main(void) {
    int x;
    int y;
    int c;

    if (c) {
        __goblint_assume(x == y);
    } else {
        __goblint_assume(x == y + 5);
    }

    __goblint_check(x - y >= 0); // SUCCESS
    __goblint_check(x - y <= 5); // SUCCESS
    __goblint_check(x == y);     // UNKNOWN!

    return 0;
}
