// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none --enable dbg.subpoly.check-invariants

#include <goblint.h>

int main(void) {
    int x, y;
    if (!(x == 1 || x == 2)) return 0;

    y = 1;
    if (x == 1) y = 2;
    // On the other branch, x != 1 trims [1,2] to the singleton [2,2].
    // Joining this interval fact with x=1,y=2 must retain x+y=3.
    __goblint_check(x + y == 3); // SUCCESS
    __goblint_check(x == 1); // UNKNOWN!
    __goblint_check(x == 2); // UNKNOWN!

    if (x == 2) y++;
    __goblint_check(y == 2); // SUCCESS
    return 0;
}
