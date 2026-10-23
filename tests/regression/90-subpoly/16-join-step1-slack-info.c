// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
SubPolyDomain.meet
SubPolyDomain.join
SubPolyDomain.add_affeq_row
SubPolyDomain.set_slack
*/

int main(void) {
    int x;
    int y;
    int z;
    int c;

    if (c) {
        __goblint_assume(x == y);
        __goblint_assume(y <= z);
    } else {
        __goblint_assume(x <= y);
        __goblint_assume(y == z);
    }

    __goblint_check(x <= y); // SUCCESS
    __goblint_check(y <= z); // SUCCESS

    return 0;
}
