// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assert_constraint
D.is_bot
SubPolyDomain.meet

Disequality guards: subpoly uses DISEQ only to detect contradiction. The
relational fact x == y is invisible to the base interval domain, so the
deadness of the x - y != 0 branch must come from subpoly.
*/

int main(void) {
    int x;
    int y;

    __goblint_assume(x == y);

    if (x - y != 0) {
        __goblint_check(0); // NOWARN (dead code)
    } else {
        __goblint_check(x == y); // SUCCESS
    }

    return 0;
}
