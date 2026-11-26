// PARAM: --set ana.activated[+] subpoly --enable ana.subpoly.basis-exploration --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
Two independent copies of the basis-exploration example, with interleaved
variable names. Each component needs combinations of its two equality rows
to materialize the bounds on w. Both have a nonzero RHS, which must not
connect otherwise independent components.

Havocking x afterwards removes the equalities needed to recover w's bounds:
the checks must use the intervals stored by exploration. The unrelated
interval on independent must also survive.
*/

extern int mystery(int v);

int main(void) {
    int w0, w1;
    int x0, x1;
    int y0, y1;
    int z0, z1;
    int independent;

    __goblint_assume(independent >= 7);
    __goblint_assume(independent <= 9);

    __goblint_assume(x0 >= 0);
    __goblint_assume(x0 <= 2);
    __goblint_assume(x1 >= 0);
    __goblint_assume(x1 <= 2);
    __goblint_assume(y0 >= 0);
    __goblint_assume(y0 <= 3);
    __goblint_assume(y1 >= 0);
    __goblint_assume(y1 <= 3);

    __goblint_assume(x0 + z0 + w0 == 1);
    __goblint_assume(x1 + z1 + w1 == 1);
    __goblint_assume(y0 + z0 - w0 == 0);
    __goblint_assume(y1 + z1 - w1 == 0);

    x0 = mystery(x0);
    x1 = mystery(x1);

    __goblint_check(w0 >= 0);  // SUCCESS
    __goblint_check(w0 <= 2);  // SUCCESS
    __goblint_check(w0 <= 1);  // UNKNOWN!
    __goblint_check(w1 >= 0);  // SUCCESS
    __goblint_check(w1 <= 2);  // SUCCESS
    __goblint_check(w1 <= 1);  // UNKNOWN!

    __goblint_check(independent >= 7);  // SUCCESS
    __goblint_check(independent <= 9);  // SUCCESS

    return 0;
}
