// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assert_constraint
D.assign_exp
SubPolyDomain.meet
SubPolyDomain.join

Compound && guards (a chain of meet_tcons on one branch edge) and a do-while
loop (body executes before the first guard evaluation).
*/

int main(void) {
    int x;
    int y;

    if (x >= 0 && x <= 10 && y == x + 5) {
        __goblint_check(y >= 5);  // SUCCESS
        __goblint_check(y <= 15); // SUCCESS
        __goblint_check(y - x == 5); // SUCCESS
    }

    int i = 0;
    do {
        i++;
    } while (i < 5);

    __goblint_check(i >= 5); // SUCCESS
    __goblint_check(i >= 1); // SUCCESS

    return 0;
}
