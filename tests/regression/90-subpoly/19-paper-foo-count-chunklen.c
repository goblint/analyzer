// PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none

#include <goblint.h>

/*
SUBPOLY-REQUIRES:
D.assign_exp
D.assert_constraint
ExpressionBounds.bound_texpr
D.invariant
*/

int main(void) {
    int wb;
    int count;
    int chunkLen;
    int arrayLen;

    __goblint_assume(wb >= 2 * count);
    __goblint_assume(count + chunkLen >= arrayLen);

    {
        int len = arrayLen - chunkLen;
        __goblint_check(wb >= 2 * len); // SUCCESS
    }

    return 0;
}
